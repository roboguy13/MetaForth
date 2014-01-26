{-# LANGUAGE DeriveFunctor #-}

-- TODO: Add support for parsing S" ..." style string literals
-- TODO: Add parse error checking

module Parser
  (WordName
  ,Code     (..)
  ,ParsedCode
  ,deannotate
  ,parse
  )
  where

import Control.Monad (void)

type WordName = String

data Code annotation =
    Word    WordName
  | WordDef WordName   [Code annotation]
  | Quoted  annotation [Code annotation]
  deriving (Show, Functor)

type ParsedCode = Code ()

deannotate :: Code a -> Code ()
deannotate = void

parse :: String -> Maybe [ParsedCode]
parse = Just . fst . go . concatMap brackets . words
  where
    go :: [String] -> ([ParsedCode], [String])
    go []       = ([], [])

    -- Word definition
    go (":":w:ws) =
      let (block, rest)     = go ws
          (restCode, rest') = go rest
      in
      (WordDef w block : restCode, rest')

--    go (":":_) =
--      Nothing      -- This must be a syntax error in the input

    -- Quoting
    go ("[":ws) =
      let (block, rest)     = go ws
          (restCode, rest') = go rest
      in
      (Quoted () block : restCode, rest')

    go (";":rest)  =
      ([], rest)

    go ("]":rest)  =
      ([], rest)

    -- Comments
    go ("(":ws) =
      let (_, rest) = break (== ")") ws
      in
      case rest of
        []        -> ([], [])
        (_:rest') ->
          go rest'

    -- Word
    go (w  :ws) =
      let (restCode, rest) = go ws
      in
      (Word w : restCode, rest)


brackets :: String -> [String]
brackets []       = []
brackets ('[':cs) = "[" : brackets cs
brackets (']':cs) = "]" : brackets cs
brackets cs       =
  let (word, rest) = break (`elem` "[]") cs
  in
  word : brackets rest
