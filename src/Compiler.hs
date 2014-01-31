{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

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

-- NOTE: compileBuiltins and generateThunks are independent from each other.
compileCode :: Annotate a => String -> [Code a] -> (String -> String) -> String
compileCode label parsed transform =
  let annotated'    = annotateCode         parsed
      annotated     = map (fmap transform) annotated'
      (names, defs) = generateWordDefs     annotated
      thunks        = generateThunks       annotated
  in
  thunks ++ defs ++ label ++ compileCalls names annotated



-- TODO: Find a cleaner way. The type class ensures we only mangle things once.

-- | Annotate the code with mangled thunk names and mangle user defined words
class Annotate a where
  annotateCode :: [Code a] -> [AnnotatedCode]

instance Annotate () where
  annotateCode =
    flip evalState (M.empty, 0) . mapM annotate
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

      annotate (Word wordName) =
        mangleIt wordName (return . Word)

      annotate (WordDef wordName code) =
        mangleIt wordName
                 (\mangled ->
                    WordDef mangled <$> mapM annotate code)


      annotate (Quoted () code) = do
        modify (second (+1))
        (_, n) <- get
        let mangled = mangleThunk n
        Quoted mangled <$> mapM annotate code

instance Annotate [Char] where
  annotateCode = id




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

--unmangleWordName :: MangledName -> String
--unmangleWordName = drop (length wordMangling)

-- TODO: Finish
isLiteral :: String -> Bool
isLiteral = all isNumber

-- | Thunk and word definition compilation stage
-- Clobbers rdx
compileLabel :: Annotate a => String -> [Code a] -> String
compileLabel mangledName code =
   compileCode
    (mangledName ++ ":")
    code
    prefix
     ++ "ret\n\n"
  where
    prefix = (++ mangledName)
 

generateLabels :: Annotate a => (Code a -> Maybe (WordName, [Code a])) -> [Code a] -> ([String], String)
generateLabels fn =
  mconcat . map go
  where
    go x =
      case fn x of
        Nothing           -> ([], "")
        Just (name, code) ->
          ([name], compileLabel name code)


generateThunks :: [AnnotatedCode] -> String
generateThunks = snd . generateLabels thunk
  where
    thunk (Quoted name code) = Just (name, code)
    thunk _                  = Nothing

generateWordDefs :: forall a. Annotate a => [Code a] -> ([String], String)
generateWordDefs = generateLabels wordDef
  where
    wordDef :: Code a -> Maybe (WordName, [Code a])
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
  ,("rot",  popInto "rax" ++ popInto "rbx" ++ popInto "rcx" ++ push "rbx" ++ push "rax" ++ push "rcx")
  ,("+",    popInto "rax" ++ popInto "rbx" ++ "add rax, rbx\n" ++ push "rax")
  ,("-",    popInto "rax" ++ popInto "rbx" ++ "sub rbx, rax\n" ++ push "rbx")
  ,("*",    popInto "rax" ++ popInto "rbx" ++ "mul rbx\n"      ++ push "rax")
  ,("/",    popInto "rax" ++ popInto "rbx" ++ "div rax, rbx\n" ++ push "rax")
  ,("call", popInto "rax" ++ "\ncall rax\n")
  ]

-- | Finish compiling thunks (i.e. pop the addresses in the appropriate place)
--   and compile calls to builtin words
-- TODO: Check for undefined words
--   *** Might clobber rcx
compileCalls :: [WordName] -> [AnnotatedCode] -> String
compileCalls _ =
  concatMap go
  where
    userDefined wordName
        = "call " ++ wordName ++ "\n"

    go (Word wordName)
      | all isNumber wordName = compileIntLiteral wordName
      | otherwise             =
        fromMaybe (userDefined wordName)
                  (lookup wordName builtins)
    go (Quoted name _) = "lea rcx, [rel " ++ name ++ "]" ++ push "rcx" ++ "\n"
    go _ = ""
