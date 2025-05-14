module Pcl where

type Number = Int
type VarName = String
type FuncName = String

data Loc = Pilha Number | Memoria Number
    deriving (Show, Eq)

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

data ErrorKinds = UserError 
    | Any 
    | UninitializedMemoryAcess 
    | UninitializedMemoryWrite
    | InitializedButEmptyMemoryAcess 
    | ControlValueStackAcess
    | UninitializedStackAcess
    | UninitializedStackWrite
    | InitializedButEmptyStackAcess 
    deriving (Show)


data Exp = Var VarName
    | Value Value
    | Binop Binop Exp Exp
    | Not Exp
    | Deref Exp
    | Ref VarName
    | Comp Exp Exp
    | Scope Exp -- equivalente a {}
    | Pop Exp -- equivalente a escopo e
    | Malloc Exp 
    | Free Exp Exp 
    | Let VarName Number
    | Assign Exp Exp
    | If Exp Exp Exp
    | While Exp Exp
    | CallFunc FuncName [Exp]
    | Fpop Exp -- marcador de escopo de função
    | Panic ErrorKinds


instance Show Exp where
    show base_expr = case base_expr of
        Var name -> name
        Value value -> show value
        Binop op expr1 expr2 -> "(" ++ show expr1 ++ show op ++ show expr2 ++ ")"
        Not expr -> "!(" ++ show expr ++ ")"
        Deref expr -> "*(" ++ show expr ++ ")"
        Ref name -> "&" ++ name
        Comp expr1 expr2 -> show expr1 ++ ";\n" ++ show expr2
        Scope expr -> "{" ++ show expr ++"}" -- equivalente a {}
        Pop expr -> "pop " ++ show expr 
        Malloc expr -> "malloc(" ++ show expr ++ ")" 
        Free expr1 expr2 -> "free(" ++ show expr1 ++ ", " ++ show expr2 ++ ")"
        Let name num -> "let " ++ name ++ "[" ++ show num ++ "]"
        Assign expr1 expr2 -> show expr1 ++ " := " ++ show expr2 
        If expr1 expr2 expr3 -> "if(" ++ show expr1 ++ ")\n(" ++ show expr2 ++ ")\nelse (" ++ show expr3 ++ ")"
        While expr1 expr2 -> "while( " ++ show expr1 ++ " )\n" ++ show expr2
        CallFunc name exprs -> name ++ "(" ++ 
            Prelude.drop 2 (Prelude.foldl (\acc expr -> acc ++ ", " ++ show expr) "" exprs) 
            ++ ")" 
        Fpop expr -> "Fpop " ++ show expr 
        Panic kind -> "panic " ++ show kind


data Function = DeclFunc FuncName [VarName] Exp Function | Main Exp
    deriving (Show)

data Global = DeclGlobal VarName Number Global | Func Function
    deriving (Show)

type Ast = Global