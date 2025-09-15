import System.Environment (getArgs)
import qualified Parser (parser)
import qualified Lexer(lexer)
import Interpreter (stepRun, fullRun)

import TypeCheck (checkProgram, testMain)
import qualified FParser (parser)
import qualified FLexer (lexer)
import qualified Compilation (compile)

compile :: String -> String -> IO()
compile inputName outputName = do
    content <- readFile inputName
    case FLexer.lexer content of 
        Left err -> do print "Lexer Error: "; print err
        Right tokens -> case FParser.parser tokens of
            Left err -> do print "parser Error: "; print err
            Right ast -> case checkProgram ast of 
                Left err -> do print "type Error: "; print err
                Right _ -> writeFile outputName (show $ Compilation.compile ast)

execute :: String -> String -> IO()
execute mode inputName = do 
    content <- readFile inputName
    case Lexer.lexer content of
        Left err -> do print "Lexer Error: "; print err
        Right tokens -> case Parser.parser tokens of
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
        (mode : rest) -> do
            run mode rest 
        _ -> putStrLn "Unrecognized arguments"
    where 
        run "compile" (arg1:arg2:_) = compile arg1 arg2
        run "execute" (arg1:arg2:_) = execute arg1 arg2
        run "test" _ = testMain
        run _ _ = error "Unrecognized mode of operation"


{- 
  cabal run porcelain 
    - compile .fpcl name.pcl
        - lex and parse file to AST 
            - type check the code
            - run compile
            - save the file
    - execute (full|step) .pcl
-}
