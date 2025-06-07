{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module PclFront where

import GHC.Utils.Misc (dropTail, filterOut)
import Data.List
import Data.Bifunctor
import Control.Applicative
import Data.Maybe

concatComma :: Show a => [a] -> String
concatComma = dropTail 2 . concatMap ((++", ") . show)

type Number = Int
type VarName = String
type FuncName = String

data Local = Pilha | Memoria
    deriving (Show, Eq)

type Loc = (Local, Int) 

data Value = Number Number | Loc Loc
    deriving (Show, Eq)

data Binop = Add | Sub | Mult | Less | Greater | Equal | And | Or 

instance Show Binop where
    show op = case op of
        Add -> " + "
        Sub -> " - "
        Mult -> " * "
        Less -> " < " 
        Greater -> " > " 
        Equal -> " = " 
        And -> " & " 
        Or -> " | " 

type Reg = Int

-- TODO: Lidar com os tipos recursivos e as limitações
data Tipo = Num | TupleT [Tipo] | Ptr Tipo | AliasT Tipo Reg | Rec (Tipo -> Tipo) | W | Fn [Tipo] Tipo

instance Show Tipo where
    show t = case t of
        Num -> "Int"
        TupleT list -> concatMap ((++", ") . show) list
        Ptr t1 -> "*" ++ show t1
        AliasT t1 reg -> "@" ++ show t1 ++ "'" ++ show reg 
        Rec f -> "uw. " ++ show (f W)
        W -> "w"
        Fn args ret -> "(" ++ concatComma args ++ ") -> " ++ show ret

instance Eq Tipo where 
    (==) t1 t2 = case t1 of
        Num -> case t2 of Num -> True; _ -> False
        TupleT list1 -> case t2 of TupleT list2 -> list1 == list2; _ -> False
        Ptr t' -> case t2 of Ptr t'' -> t' == t''; _ -> False
        AliasT t' _ -> case t2 of AliasT t'' _ -> t' == t''; _ -> False 
        Fn args ret -> case t2 of Fn args' ret' -> args == args' && ret == ret'; _ -> False

        W -> case t2 of W -> True; _ -> False
        Rec f -> case t2 of Rec f' -> f W == f' W; _ -> False  


data Exp = Var VarName
    | Value Value
    | Binop Binop Exp Exp
    | Inc Exp Exp
    | Dec Exp Exp
    | Not Exp
    | Deref Exp
    | Alias Exp
    | AliasDeref Exp
    | Comp Exp Exp
    | Scope Exp
    | New Tipo Exp 
    | Delete Exp Exp 
    | Tuple [Exp]
    | LetTuple [VarName] Tipo Exp
    | LetVar VarName Tipo Exp
    | Assign Exp Exp
    | Swap Exp Exp
    | SwapDeref Exp Exp
    | If Exp Exp Exp
    | While Exp Exp
    | CallFunc FuncName [Exp]
    | Stop
    | NullPtr Tipo


instance Show Exp where
    show base_expr = case base_expr of
        Var name -> name
        Value value -> show value
        Binop op expr1 expr2 -> "(" ++ show expr1 ++ show op ++ show expr2 ++ ")"
        Inc expr1 expr2 -> show expr1 ++ " += " ++ show expr2
        Dec expr1 expr2 -> show expr1 ++ " -= " ++ show expr2
        Not expr -> "!(" ++ show expr ++ ")"
        Deref expr -> "*(" ++ show expr ++ ")"
        Alias expr -> "alias ("++ show expr ++")"
        AliasDeref expr -> "alias* ("++ show expr ++")"
        Comp expr1 expr2 -> show expr1 ++ ";\n" ++ show expr2
        Scope expr -> "{\n"++ show expr ++ "\n}"
        New t expr  -> "new<" ++ show t ++ ">(" ++ show expr ++ ")" 
        Delete expr1 expr2 -> "delete(" ++ show expr1 ++ ", " ++ show expr2 ++ ")"
        Tuple exprs -> "(" ++ concatComma exprs ++ "))"
        LetTuple vars t expr  -> "let (" ++ concatComma vars ++ "): " ++ show t ++ " := (" ++ show expr ++ ")"
        LetVar name t expr -> "let " ++ name ++ ": " ++ show t ++ " := (" ++ show expr ++ ")"
        Assign expr1 expr2 -> show expr1 ++ " := " ++ show expr2 
        Swap expr1 expr2 -> show expr1 ++ " :=: " ++ show expr2 
        SwapDeref expr1 expr2 -> show expr1 ++ " :=:* " ++ show expr2 
        If expr1 expr2 expr3 -> "if(" ++ show expr1 ++ ")\n(" ++ show expr2 ++ ")\nelse (" ++ show expr3 ++ ")"
        While expr1 expr2 -> "while( " ++ show expr1 ++ " )\n" ++ show expr2
        CallFunc name exprs -> name ++ "(" ++ concatComma exprs ++ ")" 
        Stop -> "stop"
        NullPtr t -> "nullptr<" ++ show t ++ ">"


data Function = DeclFunc FuncName [(VarName, Tipo)] Tipo Exp Function | Main Exp
    deriving (Show)

data Global = DeclGlobal VarName Tipo Value Global | Func Function
    deriving (Show)

type Ast = Global

isPtr :: Tipo -> Bool
isPtr = \case 
    Ptr _ -> True
    AliasT _ _ -> True
    _ -> False

isLinear :: Tipo -> Bool
isLinear = \case
    Ptr _ -> True
    TupleT list -> any isLinear list 
    _ -> False

data TypeError = UseOfMovedValue | UnmatchedTypes | Unimplemented | MissingType | BareLoc

data Env = EnvT {
    gamma :: [(VarName, Tipo)],
    used :: [(VarName, Tipo)]
}

-- continue : matém o used e o gamma como estão

-- repairU: junta o gamma e o used
repairU :: Env -> Env
repairU EnvT { gamma, used } = EnvT {gamma = gamma ++ used, used = []}

-- discard: discarta o used
discardU :: Env -> Env
discardU EnvT { gamma } = EnvT { gamma, used = [] }

-- unite: faz a intersecção gamma1, gamma2
interseccao :: Env -> Env -> Env
interseccao EnvT { gamma=gamma1 } EnvT { gamma=gamma2 } = EnvT { gamma = gamma1 `intersect` gamma2, used = []}

diff :: Env -> Env -> Env
EnvT { gamma=gamma1 } `diff` EnvT{gamma = gamma2} = EnvT { gamma = filterOut (`elem` gamma2) gamma1, used = [] }


-- checa se todos os elementos em gamma são não lineares
nonlinear :: Env -> Bool
nonlinear EnvT { gamma } = any (isLinear . snd) gamma

-- has: checa se gamma tem x, retornando o tipo ou error
-- isso move x de gamma para used
getVar :: VarName -> Env -> Either TypeError (Env, Tipo) 
getVar name EnvT { gamma, used } = do 
    (gamma', tipo) <- pop gamma
    Right (EnvT { gamma = gamma', used = (name, tipo) : used }, tipo)
    where
        -- pega o elemento com o nome e remove de gamma
        pop :: [(VarName, Tipo)] -> Either TypeError ([(VarName, Tipo)], Tipo)
        pop ((n, tipo) : resto)
            | n == name = Right (resto, tipo)
            | otherwise = do 
                (gamma', t) <- pop resto
                Right ((n, tipo) : gamma', t)
        pop [] = Left UseOfMovedValue

-- add: adiciona x de tipo T em gamma
addVar :: (VarName, Tipo) -> Env -> Env
addVar item EnvT { gamma, used } = EnvT { gamma = item : gamma, used }

check :: (Env, Exp) -> Either TypeError (Env, Tipo)
-- rule for var
check (env, Var x) = getVar x env

-- rule for value
check (env, Value (Number _)) = Right (env, Num)

-- NOTE: tiping of location cannot be know if it is bare
check (_, Value (Loc _)) = Left BareLoc

-- TODO: check if the operand is valid for the types
check (env, Binop op e1 e2) = do
    (env', t1) <- check (env, e1)
    (env'', t2) <- check (env', e2)
    case t1 of
        Num -> case t2 of 
            Num -> Right (env'', Num)
            Ptr t -> Right (env'', Ptr t)
            AliasT t reg -> Right (env'', AliasT t reg)
            _ -> Left UnmatchedTypes
        Ptr t -> case t2 of
            Num -> Right (env'', Ptr t)
            _ -> Left UnmatchedTypes
        AliasT t reg -> case t2 of 
            Num -> Right (env'', AliasT t reg)
            _ -> Left UnmatchedTypes
        _ -> Left UnmatchedTypes

check (env, Inc expr1 expr2) = do
    (env', t) <- check (env, expr1)
    if case t of Ptr _ -> True; AliasT _ _ -> True; _ -> False then do
        (env2, t2) <- check (repairU env', expr2)
        if t2 == Num then 
            Right (discardU env2, TupleT [])
        else
            Left UnmatchedTypes
    else
        Left UnmatchedTypes

check (env, Dec expr1 expr2) = do
    (env', t) <- check (env, expr1)
    if case t of Ptr _ -> True; AliasT _ _ -> True; _ -> False then do
        (env2, t2) <- check (repairU env', expr2)
        if t2 == Num then 
            Right (discardU env2, TupleT [])
        else
            Left UnmatchedTypes
    else
        Left UnmatchedTypes

check (env, Not expr) = do
    (env', t) <- check (env, expr)
    case t of 
        Num -> Right (env', Num)
        AliasT _ _ -> Right (env', Num)
        _ -> Left UnmatchedTypes

check (env, Deref expr) = do 
    (env', t) <- check (env, expr)
    Right (repairU env', t)

-- get alias of ptr type
check (env, Alias expr) = do
    (env', t) <- check (env, expr)
    case t of
        Ptr t' -> Right (repairU env', AliasT t' 0)
        _ -> Left UnmatchedTypes

check (env, AliasDeref expr) = do
    (env', t) <- check (env, expr)
    case t of
        Ptr t' -> case t' of 
            Ptr _ -> Right (repairU env', AliasT t' 0)
            _ -> Left UnmatchedTypes
        _ -> Left UnmatchedTypes

check (env, Comp expr1 expr2) = do
    (env', _) <- check (env, expr1)
    check (env', expr2)

-- TODO: checar se env' não contém tipos lineares
check (env, Scope expr) = do
    (env', t) <- check (env, expr)
    Right (discardU env', t)

check (env, New t expr) = do
    (env', t1) <- check (env, expr)
    if t1 == Num then 
        Right (discardU env', Ptr t)
    else
        Left UnmatchedTypes

-- TODO: check if location has type t1
check (env, Delete expr1 expr2) = do
    (env', t1) <- check (env, expr1)
    (env'', t2) <- check (discardU env', expr2)
    if t2 == Num then 
        Right (discardU env'', TupleT [])
    else
        Left UnmatchedTypes

-- o código é, aplica fmap TupleT a saída de foldr
-- no caso de Either (saida de foldr), <$> (fmap) aplica no Right
check (env, Tuple exprs) = fmap TupleT <$> foldr pass (Right (env, [])) exprs
    where 
        pass e (Right (envAcc, list)) = do
            (envAcc', t) <- check (envAcc, e)
            Right (discardU envAcc', t : list) 
        pass _ (Left err) = Left err

-- TODO: checar se tá certa a adição de variables to Gamma
check (env, LetTuple names t expr) = do
    (env', t') <- check (env, expr)
    case t of 
        TupleT tipos -> 
            if t == t' then 
                Right (foldr addVar env' (zip names tipos), TupleT []) 
            else 
                Left UnmatchedTypes
        _ -> Left UnmatchedTypes

--TODO: usar o tipo da expressão?
check (env, LetVar name t expr) = do
    (env', t') <- check (env, expr)
    if t == t' then 
        Right (addVar (name, t) (discardU env'), TupleT [])
    else
        Left UnmatchedTypes

-- TODO: validar que t e t' são não lineares
check (env, Assign expr1 expr2) = do
    (env', t) <- check (env, expr1)
    (env'', t') <- check (discardU env', expr2)
    if t == t' then 
        Right (discardU env'', TupleT [])
    else
        Left UnmatchedTypes

-- TODO: validar o uso de tipos lineares
check (env, Swap expr1 expr2) = do
    (env', t) <- check (env, expr1)
    (env'', t') <- check (repairU env', expr2)
    if t == t' then 
        Right (repairU env'', TupleT [])
    else
        Left UnmatchedTypes

check (env, SwapDeref expr1 expr2) = do
    (env', t) <- check (env, expr1)
    (env'', t') <- check (repairU env', expr2)
    case t' of 
        Ptr t'' -> if t == t'' then 
            Right (repairU env'', TupleT [])
        else
            Left UnmatchedTypes
        _ -> Left UnmatchedTypes

-- NOTE: imagina-se que usando o Scope ali ele elimina as lineares e faz o check automático
check (env, If expr1 expr2 expr3) = do 
    (env', t) <- check (env, expr1) 
    if t == Num then do
        (env2, t2) <- check (env', Scope expr2) 
        (env3, t3) <- check (env', Scope expr3) 
        if t2 == t3 then 
            Right (interseccao env2 env3, t3) 
        else 
            Left UnmatchedTypes
    else
        Left UnmatchedTypes

-- retornar unit
check (env, While expr1 expr2) = do 
    (env', t) <- check (env, expr1) 
    if t == Num then
        fmap (seq $ TupleT []) <$> check (env', Scope expr2)
    else
        Left UnmatchedTypes

-- TODO 
check (env, CallFunc name args) = Left Unimplemented

check (env, Stop) = Right (env, TupleT [])

check (env, NullPtr tipo) = Right (env, Ptr tipo)