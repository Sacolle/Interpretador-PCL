import Lexer(Token, lexer, hack)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Parser (parser)
import Porcelain (stepRun, fullRun)


run:: String -> String -> IO()
run "full" content = 
    case lexer content of
        Left err -> do print "Lexer Error: "; print err
        Right tokens -> case parser tokens of
            Left err -> do print "parser Error: "; print err
            Right ast -> fullRun ast


main :: IO ()
main = do
    stringArgs <- getArgs
    case stringArgs of
        (mode : file : _) -> do
            content <- readFile file
            run mode content
        _ -> putStrLn "Unrecognized arguments"


