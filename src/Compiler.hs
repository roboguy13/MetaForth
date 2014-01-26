module Compiler
  (compile
  )
  where

import           Parser

import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as M

import           Data.Char  (isNumber)
import           Data.Maybe (fromMaybe)

import           Control.Applicative

import           Data.Monoid


type AnnotatedCode = Code String

type MangledName   = String


-- NOTE: compileBuiltins and generateThunks are independent from each other.
compile :: [ParsedCode] -> String
compile parsed =
  unlines
    ["EXTERN _setup_stack"
    ,"BITS 64"
    ,"GLOBAL _start"
    ,"SECTION .text\n"
    ,compileCode ("\n\n\n_start:\n" ++ setupStack) parsed id
    ,exitCode
    ]
  where
    exitCode = popInto "rdi" ++ "mov rax, 0x2000001\nsyscall"

setupStack :: String
setupStack =
  unlines
    ["and rsp, -16   ; stack alignment"
    ,"call _setup_stack"
    ,"mov r9, rax"
    ,"sub qword r9, 8"
    ]

compileCode :: String -> [ParsedCode] -> (String -> String) -> String
compileCode label parsed transform =
  let annotated'    = annotateCode parsed
      annotated     = map (fmap transform) annotated'
      (names, defs) = generateWordDefs     annotated
      thunks        = generateThunks       annotated
  in
  thunks ++ defs ++ label ++ compileCalls names annotated


-- | Annotate the code with mangled thunk names and mangle user defined words
annotateCode :: [ParsedCode] -> [AnnotatedCode]
annotateCode =
  flip evalState (M.empty, 0) . mapM go
  where
    first :: (a -> a') -> (a, b) -> (a', b)
    first  f (x, y) = (f x, y)

    second :: (b -> b') -> (a, b) -> (a, b')
    second          = fmap


    mangleIt ::
          String 
          -> (String -> State (Map String MangledName, Int) AnnotatedCode) 
          -> State (Map String MangledName, Int) AnnotatedCode
    mangleIt wordName fn = do
      (mangledWords, _) <- get
      case M.lookup wordName mangledWords of
        Nothing ->
          case lookup wordName builtins of
            Nothing -> do
              let mangled = mangleWordName wordName
              modify (first $ M.insert wordName mangled)
              fn mangled

            Just _ ->
              fn wordName

        Just mangled ->
          fn mangled


    go :: ParsedCode -> State (Map String MangledName, Int) AnnotatedCode
    go (Word wordName) =
      mangleIt wordName (return . Word)

    go (WordDef wordName code) =
      mangleIt wordName
               (\mangled ->
                  WordDef mangled <$> mapM go code)


    go (Quoted () code) = do
      modify (second (+1))
      (_, n) <- get
      let mangled = mangleThunk n
      return $! Quoted mangled (annotateCode code)



mangleThunk :: Int -> String
mangleThunk n = "__thunk_" ++ show n

wordMangling :: String
wordMangling = "__userDefined_"

mangleWordName :: String -> MangledName
mangleWordName wordName
  | isLiteral wordName = wordName
  | otherwise =
    case lookup wordName builtins of
      Nothing -> wordMangling ++ wordName
      Just _  -> wordName

unmangleWordName :: MangledName -> String
unmangleWordName = drop (length wordMangling)

-- TODO: Finish
isLiteral :: String -> Bool
isLiteral = all isNumber

-- | Thunk and word definition compilation stage
-- Clobbers rdx
compileLabel :: String -> [ParsedCode] -> String
compileLabel mangledName code =
   compileCode
    (mangledName 
      ++ ":")
    code
    prefix
     ++ "ret\n\n"
  where
    prefix = (++ mangledName)
 

generateLabels :: (AnnotatedCode -> Maybe (WordName, [AnnotatedCode])) -> [AnnotatedCode] -> ([String], String)
generateLabels fn =
  mconcat . map go
  where
    go :: AnnotatedCode -> ([String], String)
    go x =
      case fn x of
        Nothing           -> ([], "")
        Just (name, code) ->
          ([name], compileLabel name (map deannotate code))


generateThunks :: [AnnotatedCode] -> String
generateThunks = snd . generateLabels thunk
  where
    thunk (Quoted name code) = Just (name, code)
    thunk _                  = Nothing

generateWordDefs :: [AnnotatedCode] -> ([String], String)
generateWordDefs = generateLabels wordDef
  where
    wordDef (WordDef name code) = Just (name, code)
    wordDef _                   = Nothing

-- TODO: Add support for non-Int literals
compileIntLiteral :: String -> String
compileIntLiteral = push

push :: String -> String
push arg =
  unlines
    ["\n\t; push " ++ arg
    ,"add qword r9, 8"
    ,"mov qword [r9], " ++ arg
    ]

popInto :: String -> String
popInto arg =
  "\n\t; pop " ++ arg ++ "\n" ++
  "mov " ++ arg ++ ", [r9]\n" ++ pop'

pop' :: String
pop' =
  "sub qword r9, 8\n"

pop :: String
pop = "\n\t; pop\n" ++ pop'

-- TODO: Clean this up
builtins :: [(String, String)]
builtins =
  [("drop", pop)
  ,("dup",  popInto "rax" ++ push "rax" ++ push "rax")
  ,("swap", popInto "rax" ++ popInto "rbx" ++ push "rax" ++ push "rbx")
  ,("+",    popInto "rax" ++ popInto "rbx" ++ "add rax, rbx\n" ++ push "rax")
  ,("-",    popInto "rax" ++ popInto "rbx" ++ "sub rbx, rax\n" ++ push "rbx")
  ,("*",    popInto "rax" ++ popInto "rbx" ++ "mul rbx\n"      ++ push "rax")
  ,("/",    popInto "rax" ++ popInto "rbx" ++ "div rax, rbx\n" ++ push "rax")
  ,("call", popInto "rax" ++ "\ncall rax\n") -- TODO: Finish testing
  ]

-- | Finish compiling thunks (i.e. pop the addresses in the appropriate place)
--   and compile calls to builtin words
--   *** Might clobber rcx
compileCalls :: [WordName] -> [AnnotatedCode] -> String
compileCalls userDefinedWords =
  concatMap go
  where
    userDefined wordName
      | wordName `elem` userDefinedWords =
        "call " ++ wordName ++ "\n"
      | otherwise =
        error $ "Undefined word " ++ show (unmangleWordName wordName)

    go (Word wordName)
      | all isNumber wordName = compileIntLiteral wordName
      | otherwise             =
        fromMaybe (userDefined wordName)
                  (lookup wordName builtins)
    go (Quoted name _) = "lea rcx, [rel " ++ name ++ "]" ++ push "rcx" ++ "\n"
    go _ = ""
