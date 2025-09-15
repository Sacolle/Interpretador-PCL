{-# LANGUAGE LambdaCase #-}

module PclFront where

import GHC.Utils.Misc (dropTail)

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



-- TODO: Lidar com os tipos recursivos e as limitações
data Tipo = Num | Ptr Tipo | AliasT Tipo Reg | Fn [Tipo] Tipo

unit :: Tipo
unit = Num

instance Show Tipo where
    show t = case t of
        Num -> "Int"
        Ptr t1 -> "*" ++ show t1
        AliasT t1 reg -> "@" ++ show t1 ++ "'" ++ show reg 
        Fn args ret -> "(" ++ concatComma args ++ ") -> " ++ show ret

instance Eq Tipo where 
    (==) :: Tipo -> Tipo -> Bool
    (==) t1 t2 = case t1 of
        Num -> case t2 of Num -> True; _ -> False
        Ptr t' -> case t2 of Ptr t'' -> t' == t''; _ -> False
        AliasT t' _ -> case t2 of AliasT t'' _ -> t' == t''; _ -> False 
        Fn args ret -> case t2 of Fn args' ret' -> args == args' && ret == ret'; _ -> False


data Exp = Var VarName
    | Value Value
    | Binop Binop Exp Exp
    | Not Exp
    | Comp Exp Exp
    | Scope Exp
    | Deref Exp
    | Ref VarName
    | Alias VarName
    | AliasDeref VarName
    | Inc VarName Exp
    | Dec VarName Exp
    | LetVar VarName Tipo Exp
    | Assign Exp Exp
    | Swap VarName VarName
    | SwapDeref VarName VarName
    | If Exp Exp Exp
    | CallFunc VarName Tipo FuncName [Exp]
    | New VarName Tipo Exp 
    | Delete VarName Exp 
    | Stop
    | NullPtr Tipo
    | NullAlias Tipo


instance Show Exp where
    show base_expr = case base_expr of
        Var name -> name
        Value value -> show value
        Binop op expr1 expr2 -> "(" ++ show expr1 ++ show op ++ show expr2 ++ ")"
        Inc expr1 expr2 -> show expr1 ++ " += " ++ show expr2
        Dec expr1 expr2 -> show expr1 ++ " -= " ++ show expr2
        Not expr -> "!(" ++ show expr ++ ")"
        Ref name -> "&" ++ name
        Deref expr -> "*(" ++ show expr ++ ")"
        Alias expr -> "alias ("++ show expr ++")"
        AliasDeref expr -> "alias* ("++ show expr ++")"
        Comp expr1 expr2 -> "(" ++ show expr1 ++ ";\n" ++ show expr2 ++ ")"
        Scope expr -> "{\n"++ show expr ++ "\n}"
        New name t expr  -> "let " ++ name ++ ": " ++ show t ++ " = new(" ++ show expr ++ ")" 
        Delete expr1 expr2 -> "delete(" ++ show expr1 ++ ", " ++ show expr2 ++ ")"
        LetVar name t expr -> "var " ++ name ++ ": " ++ show t ++ " := (" ++ show expr ++ ")"
        Assign expr1 expr2 -> show expr1 ++ " := " ++ show expr2 
        Swap expr1 expr2 -> show expr1 ++ " :=: " ++ show expr2 
        SwapDeref expr1 expr2 -> show expr1 ++ " :=:* " ++ show expr2 
        If expr1 expr2 expr3 -> "if(" ++ show expr1 ++ ")\n(" ++ show expr2 ++ ")\nelse (" ++ show expr3 ++ ")"
        -- While expr1 expr2 -> "while( " ++ show expr1 ++ " )\n" ++ show expr2
        CallFunc name1 t name2 exprs -> "let " ++ name1 ++ ": " ++ show t ++ " = " ++ name2 ++ "(" ++ concatComma exprs ++ ")" 
        Stop -> "stop"
        NullPtr t -> "nullptr<" ++ show t ++ ">"
        NullAlias t -> "nullalias<" ++ show t ++ ">"


data Function = DeclFunc FuncName [(VarName, Tipo)] Tipo Exp Function | Main Exp
    deriving (Show)

-- TODO: Atualmente não se está utilizando globais
data Global = DeclGlobal VarName Tipo Value Global | Func Function
    deriving (Show)

type Ast = Function

isPtr :: Tipo -> Bool
isPtr = \case 
    Ptr _ -> True
    AliasT _ _ -> True
    _ -> False

isLinear :: Tipo -> Bool
isLinear = \case
    Ptr _ -> True
    _ -> False

type Reg = Int
