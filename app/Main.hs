import Lexer(Token, lexer, hack)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Parser (parser)
import Interpreter (stepRun, fullRun)

run:: String -> String -> IO()
run mode content = 
    case lexer content of
        Left err -> do print "Lexer Error: "; print err
        Right tokens -> case parser tokens of
            Left err -> do print "parser Error: "; print err
            Right ast -> runAst mode ast
    where 
        runAst "full" ast = fullRun ast
        runAst "step" ast = stepRun ast
        runAst _ _ = print "modo de execução não reconhecido"


main :: IO ()
main = do
    stringArgs <- getArgs
    case stringArgs of
        (mode : file : _) -> do
            content <- readFile file
            run mode content
        _ -> putStrLn "Unrecognized arguments"


