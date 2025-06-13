module Compilation (compile) where

import qualified PclFront as Front
import qualified Pcl as Back
import Control.Monad.State



-- data Function = DeclFunc FuncName [(name, Tipo)] Tipo Exp Function | Main Exp
compile :: Front.Ast -> Back.Ast
compile inp = evalState (compileGlobals inp) 0 

compileGlobals :: Front.Ast -> State Int Back.Ast
compileGlobals front = do
    f <- compileFuncs front
    return $ Back.Func f

compileFuncs :: Front.Ast -> State Int Back.Function
compileFuncs (Front.DeclFunc name args _ body nextFun) = do
    backBody <- compileExp body
    fs <- compileFuncs nextFun
    return $ Back.DeclFunc name (map fst args) (Back.Scope backBody) fs

compileFuncs (Front.Main body) = do 
    backBody <- compileExp body
    return $ Back.Main (Back.Scope backBody)

compileExp :: Front.Exp -> State Int Back.Exp
compileExp (Front.Var name) = return $ Back.Var name

compileExp (Front.Value v) = return $ Back.Value (compileValue v)

compileExp (Front.Binop op e1 e2) = do 
    be1 <- compileExp e1
    be2 <- compileExp e2
    return $ Back.Binop (compileBinop op) be1 be2

compileExp (Front.Inc e1 e2) = do
    be1 <- compileExp e1
    be2 <- compileExp e2
    return $ Back.Comp 
        (Back.Assign be1 (Back.Binop Back.Add be1 be2))
        (Back.Value (Back.Number 0))

compileExp (Front.Dec e1 e2) = do
    be1 <- compileExp e1
    be2 <- compileExp e2
    return $ Back.Comp 
        (Back.Assign be1 (Back.Binop Back.Sub be1 be2))
        (Back.Value (Back.Number 0))

compileExp (Front.Not e) = do 
    be <- compileExp e
    return $ Back.Not be

compileExp (Front.Ref name) = return $ Back.Ref name

compileExp (Front.Deref e) = do 
    be <- compileExp e
    return $ Back.Deref be

compileExp (Front.Alias name) = return $ Back.Var name

compileExp (Front.AliasDeref name) = return $ Back.Deref (Back.Var name)

compileExp (Front.Comp e1 e2) = do
    be1 <- compileExp e1
    be2 <- compileExp e2
    return $ Back.Comp be1 be2

compileExp (Front.Scope e) = do
    be <- compileExp e
    return $ Back.Scope be

compileExp (Front.New _ e) = do
    be <- compileExp e
    return $ Back.Malloc be

compileExp (Front.Delete e1 e2) = do
    be1 <- compileExp e1
    be2 <- compileExp e2
    return $ Back.Free be1 be2

compileExp (Front.LetVar name _ e) = do
    eb <- compileExp e
    return $ Back.Comp 
        (Back.Let name 1) 
        (Back.Comp
            (Back.Assign (Back.Var name) eb)
            (Back.Value (Back.Number 0))
        )

compileExp (Front.Assign e1 e2) = do 
    be1 <- compileExp e1
    be2 <- compileExp e2
    return $ Back.Assign be1 be2

compileExp (Front.Swap e1 e2) = do 
    n <- get
    let swap = "__swap__" ++ show n
    put (n + 1)
    eb1 <- compileExp e1
    eb2 <- compileExp e2
    return $ Back.Comp 
        (Back.Let swap 1)
        (Back.Comp
            (Back.Assign (Back.Var swap) eb1) 
            (Back.Comp 
                (Back.Assign eb1 eb2)
                (Back.Assign eb2 (Back.Var swap))
            )
        )

compileExp (Front.SwapDeref e1 e2) = do
    n <- get
    let swap = "__swap__" ++ show n
    put (n + 1)
    eb1 <- compileExp e1
    eb2 <- compileExp e2
    return $ Back.Comp 
        (Back.Let swap 1)
        (Back.Comp
            (Back.Assign (Back.Var swap) eb1) 
            (Back.Comp 
                (Back.Assign eb1 (Back.Deref eb2))
                (Back.Assign (Back.Deref eb2) (Back.Var swap))
            )
        )

compileExp (Front.If e1 e2 e3) = do
    be1 <- compileExp e1
    be2 <- compileExp e2
    be3 <- compileExp e3
    return $ Back.If be1 (Back.Scope be2) (Back.Scope be3)

compileExp (Front.CallFunc fname args) = do 
    bargs <- traverse compileExp args
    return $ Back.CallFunc fname bargs

compileExp Front.Stop = return $ Back.Panic Back.UserError

compileExp (Front.NullPtr _) = return $ Back.Value (Back.Loc Back.nullLoc)

compileExp (Front.NullAlias _) = return $ Back.Value (Back.Loc Back.nullLoc)

compileValue :: Front.Value -> Back.Value 
compileValue (Front.Number n) = Back.Number n
compileValue (Front.Loc fl) = Back.Loc $ compileLoc fl

compileLoc :: Front.Loc -> Back.Loc
compileLoc (Front.Pilha, num) = Back.newLoc Back.Pilha num 0 0
compileLoc (Front.Memoria, num) = Back.newLoc Back.Memoria num 0 0

compileBinop :: Front.Binop -> Back.Binop
compileBinop Front.Add = Back.Add
compileBinop Front.Sub = Back.Sub
compileBinop Front.Mult = Back.Mult 
compileBinop Front.Less = Back.Less 
compileBinop Front.Greater = Back.Greater
compileBinop Front.Equal = Back.Equal
compileBinop Front.And = Back.And
compileBinop Front.Or = Back.Or 