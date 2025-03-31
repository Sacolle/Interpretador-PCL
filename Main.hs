import Lexer(Token, lexer, hack)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Parser (parser)


-- The arguments we'll need for our program

parseArgs :: [String] -> Maybe (FilePath, String)

parseArgs (file : rest) = do
  outputFile <- listToMaybe rest
  return (file, outputFile)

parseArgs _ = Nothing


main :: IO ()
main = do
    stringArgs <- getArgs
    case parseArgs stringArgs of
        Nothing -> putStrLn "Unrecognized arguments"
        Just (file, outputFile) -> do
            content <- readFile file
            case lexer content of
                Left err -> print err
                Right tokens -> do print tokens; print $ parser tokens


