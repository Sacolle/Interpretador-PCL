{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module PclFront where

import GHC.Utils.Misc (dropTail, filterOut)
import Data.List

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

isAddOrSubBinop :: Binop -> Bool
isAddOrSubBinop = \case
    Add -> True
    Sub -> True
    _ -> False

isCompBinop :: Binop -> Bool
isCompBinop = \case
    Less -> True
    Greater -> True
    Equal -> True
    And -> True
    Or -> True
    _ -> False


type Reg = Int

-- TODO: Lidar com os tipos recursivos e as limitações
data Tipo = Num | TupleT [Tipo] | Ptr Tipo | AliasT Tipo Reg | Rec (Tipo -> Tipo) | W | Fn [Tipo] Tipo

instance Show Tipo where
    show t = case t of
        Num -> "Int"
        TupleT list -> concatComma list
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
        Comp expr1 expr2 -> "(" ++ show expr1 ++ ";\n" ++ show expr2 ++ ")"
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
    TupleT list -> any isLinear list 
    _ -> False

match :: (Show a, Eq a) => a -> a -> b -> Either TypeError b
match t1 t2 res = 
    if t1 == t2 then
        Right res
    else 
        Left $ UnmatchedTypes ("Tipo " ++ show t1 ++ " não se encaixa com " ++ show t2)

data TypeError = UseOfMovedValue | UnmatchedTypes String| Unimplemented | MissingType | BareLoc | VarNotFound | EndOfScopeWithLinVar
    deriving (Show)

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
nonlinear EnvT { gamma } = not $ any (isLinear . snd) gamma

-- useVar: checa se gamma tem x, retornando o tipo ou error
-- isso move x de gamma para used
useVar :: VarName -> Env -> Either TypeError (Env, Tipo) 
useVar name EnvT { gamma, used } = do 
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

getVar :: VarName -> Env -> Either TypeError Tipo
getVar name EnvT {gamma}= maybe (Left VarNotFound) (Right . snd) (find ((==)name . fst) gamma)

isVarLin :: VarName -> Env -> Either TypeError Bool
isVarLin name env = isLinear <$> getVar name env

-- add: adiciona x de tipo T em gamma
addVar :: (VarName, Tipo) -> Env -> Env
addVar item EnvT { gamma, used } = EnvT { gamma = item : gamma, used }

check :: (Env, Exp) -> Either TypeError (Env, Tipo)
-- rule for var
-- if var is non linear, just get the type
-- else get the type and move the var to the used space
check (env, Var x) = do
    isLin <- isVarLin x env 
    if isLin then 
        useVar x env 
    else do
        tipo <- getVar x env 
        case tipo of
            Fn _ _ -> Left $ UnmatchedTypes ("Nome de variável não pode ser usada como variável")
            _ -> Right (env, tipo)


-- rule for value
check (env, Value (Number _)) = Right (env, Num)

-- NOTE: tiping of location cannot be know if it is bare
check (_, Value (Loc _)) = Left BareLoc

check (env, Binop op e1 e2) = do
    (env', t1) <- check (env, e1)
    (env'', t2) <- check (discardU env', e2)
    (discardU env'', ) <$> binopParse t1 t2
    where
        binopParse t1 t2 
            | isAddOrSubBinop op = 
                case t1 of
                    Num -> case t2 of 
                        Num -> Right Num
                        Ptr t -> Right (Ptr t)
                        AliasT t reg -> Right (AliasT t reg)
                        rest -> Left $ UnmatchedTypes ("Em adição ou subtração " ++ show rest ++ " não encaixa com Num")
                    Ptr t -> case t2 of
                        Num -> Right (Ptr t)
                        rest -> Left $ UnmatchedTypes ("Em adição ou subtração " ++ show rest ++ " não encaixa com Ptr")
                    AliasT t reg -> case t2 of 
                        Num -> Right (AliasT t reg)
                        rest -> Left $ UnmatchedTypes ("Em adição ou subtração " ++ show rest ++ " não encaixa com Alias")
                    rest -> Left $ UnmatchedTypes ("Adição ou subtração não está definido par tipo " ++ show rest)
            | isCompBinop op = 
                case t1 of
                    Num -> case t2 of 
                        Num -> Right Num
                        rest -> Left $ UnmatchedTypes ("Em Comparação " ++ show rest ++ " não encaixa com Num")
                    Ptr t -> case t2 of
                        Ptr t' | t == t' -> Right Num
                        rest -> Left $ UnmatchedTypes ("Em Comparação " ++ show rest ++ " não encaixa com *" ++ show t)
                    AliasT t reg -> case t2 of -- NOTE: sub regions
                        AliasT t' reg' | t == t' && reg == reg' -> Right Num
                        rest -> Left $ UnmatchedTypes ("Em Comparação " ++ show rest ++ " não encaixa com @" ++ show t ++ "'" ++ show reg)
                    rest -> Left $ UnmatchedTypes ("Comparação não está definido para tipo " ++ show rest)
            | otherwise = -- exclusivamente Mult
                if t1 == Num && t2 == Num then 
                    Right Num
                else 
                    Left $ UnmatchedTypes ("Comparação não está definido para os tipos " ++ show t1 ++ ", " ++ show t2)

check (env, Inc expr1 expr2) = do
    (env', t) <- check (env, expr1)
    if isPtr t then do
        (env2, t2) <- check (repairU env', expr2)
        match t2 Num (discardU env2, TupleT [])
    else
        Left $ UnmatchedTypes ("Operador da esquerda deve ser um ponteiro, o atual é " ++ show t)

check (env, Dec expr1 expr2) = do
    (env', t) <- check (env, expr1)
    if isPtr t then do
        (env2, t2) <- check (repairU env', expr2)
        match t2 Num (discardU env2, TupleT [])
    else
        Left $ UnmatchedTypes ("Operador da esquerda deve ser um ponteiro, o atual é " ++ show t)

check (env, Not expr) = do
    (env', t) <- check (env, expr)
    case t of 
        Num -> Right (discardU env', Num)
        AliasT _ _ -> Right (discardU env', Num)
        _ -> Left $ UnmatchedTypes ("Operador not só opera sobre ponteiros e aliases, o atual é " ++ show t)

-- deref *T -> T if T is nonlinear
check (env, Deref expr) = do 
    (env', t) <- check (env, expr)
    case t of 
        Ptr innerT | not $ isLinear innerT -> Right (repairU env', innerT)
        _ -> Left $ UnmatchedTypes ("Só pode-se desreferenciar ponteiros para valores não lineares. O atual é " ++ show t)

-- get alias of ptr type
check (env, Alias expr) = do
    (env', t) <- check (env, expr)
    case t of
        Ptr t' -> Right (repairU env', AliasT t' 0)
        _ -> Left $ UnmatchedTypes ("Só pode-se fazer alias de ponteiros. O atual é " ++ show t)

check (env, AliasDeref expr) = do
    (env', t) <- check (env, expr)
    case t of
        Ptr t' -> case t' of 
            Ptr _ -> Right (repairU env', AliasT t' 0)
            _ -> Left $ UnmatchedTypes ("Só pode-se fazer alias deref de ponteiros a ponteiros. O atual é " ++ show t)
        _ -> Left $ UnmatchedTypes ("Só pode-se fazer alias de ponteiros. O atual é " ++ show t)

check (env, Comp expr1 expr2) = do
    (env', _) <- check (env, expr1)
    check (discardU env', expr2)

-- env'/env são as variáveis declaradas em env' que ainda estão vivas
-- se todas elas nesse conjunto são não lineares
-- retorna env `intersect` env' -> 
-- isso resulta nas vars que existiam em env menos as vars de env' e os lineares consumidos em env'
check (env, Scope expr) = do
    (env', t) <- check (env, expr)
    if nonlinear (env' `diff` env) then 
        Right (env `interseccao` env', t)
    else
        Left EndOfScopeWithLinVar

check (env, New t expr) = do
    (env', t1) <- check (env, expr)
    match t1 Num (discardU env', Ptr t)

check (env, Delete expr1 expr2) = do
    (env', _) <- check (env, expr1)
    (env'', t2) <- check (discardU env', expr2)
    match t2 Num (discardU env'', TupleT [])

-- o código é, aplica fmap TupleT a saída de foldr
-- no caso de Either (saida de foldr), <$> (fmap) aplica no Right
check (env, Tuple exprs) = fmap TupleT <$> foldr pass (Right (env, [])) exprs
    where 
        pass e (Right (envAcc, list)) = do
            (envAcc', t) <- check (envAcc, e)
            Right (discardU envAcc', t : list) 
        pass _ (Left err) = Left err

-- NOTE: checar se tá certa a adição de variables to Gamma
check (env, LetTuple names t expr) = do
    (env', t') <- check (env, expr)
    case t of 
        TupleT tipos -> match t t' (foldr addVar env' (zip names tipos), TupleT []) 
        _ -> Left $ UnmatchedTypes ("Tipo do lado esquerdo deve ser uma tupla. Atual é " ++ show t)

--NOTE: usar o tipo da expressão?
check (env, LetVar name t expr) = do
    (env', t') <- check (env, expr)
    match t t' (addVar (name, t) (discardU env'), TupleT [])

check (env, Assign expr1 expr2) = do
    (env', t) <- check (env, expr1)
    (env'', t') <- check (discardU env', expr2)
    if not (isLinear t) && not (isLinear t') then 
        match t t' (discardU env'', TupleT [])
    else
        Left $ UnmatchedTypes ("Os dois tipos da atribuição devem ser não lineares. Os atuais são " ++ show t ++ ", " ++ show t')

-- NOTE: validar o uso de tipos lineares
check (env, Swap expr1 expr2) = do
    (env', t) <- check (env, expr1)
    (env'', t') <- check (repairU env', expr2)
    match t t' (repairU env'', TupleT [])

check (env, SwapDeref expr1 expr2) = do
    (env', t) <- check (env, expr1)
    (env'', t') <- check (repairU env', expr2)
    case t' of 
        Ptr t'' -> match t t'' (repairU env'', TupleT [])
        _ -> Left $ UnmatchedTypes ("Elemento do lado direito deve ser um ponteiro para um ponteiro. Atual é " ++ show t')

-- NOTE: imagina-se que usando o Scope ali ele elimina as lineares e faz o check automático
check (env, If expr1 expr2 expr3) = do 
    (env', t) <- check (env, expr1) 
    if t == Num then do
        (env2, t2) <- check (env', Scope expr2) 
        (env3, t3) <- check (env', Scope expr3) 
        match t2 t3 (env2 `interseccao` env3, t3) 
    else
        Left $ UnmatchedTypes ("Tipo da expressão de comparação deve ser inteiro. Atual é " ++ show t)

-- retornar unit
check (env, While expr1 expr2) = do 
    (env', t) <- check (env, expr1) 
    if t == Num then
        -- apply to the right side of either (<$>)
        -- replace the snd element with TupleT [] (<$)
        -- isso pois funtores são aplicados ao segundo elemento das tuplas
        (<$) (TupleT []) <$> check (env', Scope expr2)
    else
        Left $ UnmatchedTypes ("Tipo da expressão de comparação deve ser inteiro. Atual é " ++ show t)

-- TODO 
check (env, CallFunc name args) = do
    tipo <- getVar name env
    case tipo of
        Fn tArgs ret -> do
            (resEnv, tipoDosArgs) <- foldr pass (Right (env, [])) args
            match tipoDosArgs tArgs (resEnv, ret) 
        rest -> Left $ UnmatchedTypes ("O nome da função não está associado a uma função. Esta associado a " ++ show rest)
    where 
        pass e (Right (envAcc, list)) = do
            (envAcc', t) <- check (envAcc, e)
            Right (discardU envAcc', t : list) 
        pass _ (Left err) = Left err

check (env, Stop) = Right (env, TupleT [])

check (env, NullPtr tipo) = Right (env, Ptr tipo)


checkFn :: (Env, Function) -> Either TypeError (Env, Tipo)
checkFn (env, DeclFunc name args retT body nextFn) = do
    -- adiciona o tipo da função na lista antes de validar, pois o corpo pode chamar a função recursivamente
    (env', t) <- check (addVar (name, Fn (map snd args) retT) env, body)
    if t == retT then 
        checkFn (env', nextFn) 
    else
        Left $ UnmatchedTypes ("Tipo de retorno não é igual ao tipo do corpo. Retorno é " ++ show retT ++", mas o corpo é " ++ show t)

checkFn (env, Main body) = check (env, Scope body)


checkProgram :: Ast -> Either TypeError Tipo
checkProgram ast = snd <$> checkFn (EnvT { gamma = [], used = []}, ast)

(.>) :: Exp -> Exp -> Exp
(.>) = Comp

infixr 8 .> 


ex1 = Main (
    LetVar "x" (Ptr $ Ptr Num) (New (Ptr Num) (Value (Number 1))) .>
    LetVar "y" (AliasT (Ptr Num) 0) (Alias (Var "x")) .>
    Var "y" .> 
    Deref (Var "x")
    )

testMain :: IO ()
testMain = do
    print ex1
    print $ checkProgram ex1