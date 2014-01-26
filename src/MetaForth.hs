import Parser
import Compiler

main :: IO ()
main = do
  code <- getContents

  case parse code of
    Nothing     -> error "Parse error"
    Just parsed -> putStrLn $ compile parsed
