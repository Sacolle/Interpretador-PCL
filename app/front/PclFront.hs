{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module PclFront where

import GHC.Utils.Misc (dropTail)
import Data.Function

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

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
    (==) t1 t2 = case t1 of
        Num -> case t2 of Num -> True; _ -> False
        Ptr t' -> case t2 of Ptr t'' -> t' == t''; _ -> False
        AliasT t' _ -> case t2 of AliasT t'' _ -> t' == t''; _ -> False 
        Fn args ret -> case t2 of Fn args' ret' -> args == args' && ret == ret'; _ -> False


data Exp = Var VarName
    | Value Value
    | Binop Binop Exp Exp
    | Inc Exp Exp
    | Dec Exp Exp
    | Not Exp
    | Ref VarName
    | Deref Exp
    | Alias VarName
    | AliasDeref VarName
    | Comp Exp Exp
    | Scope Exp
    | New Tipo Exp 
    | Delete Exp Exp 
    | LetVar VarName Tipo Exp
    | Assign Exp Exp
    | Swap Exp Exp
    | SwapDeref Exp Exp
    | If Exp Exp Exp
    | While Exp Exp
    | CallFunc FuncName [Exp]
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
        New t expr  -> "new<" ++ show t ++ ">(" ++ show expr ++ ")" 
        Delete expr1 expr2 -> "delete(" ++ show expr1 ++ ", " ++ show expr2 ++ ")"
        LetVar name t expr -> "let " ++ name ++ ": " ++ show t ++ " := (" ++ show expr ++ ")"
        Assign expr1 expr2 -> show expr1 ++ " := " ++ show expr2 
        Swap expr1 expr2 -> show expr1 ++ " :=: " ++ show expr2 
        SwapDeref expr1 expr2 -> show expr1 ++ " :=:* " ++ show expr2 
        If expr1 expr2 expr3 -> "if(" ++ show expr1 ++ ")\n(" ++ show expr2 ++ ")\nelse (" ++ show expr3 ++ ")"
        While expr1 expr2 -> "while( " ++ show expr1 ++ " )\n" ++ show expr2
        CallFunc name exprs -> name ++ "(" ++ concatComma exprs ++ ")" 
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

match :: (Show a, Eq a) => a -> a -> b -> Either TypeError b
match t1 t2 res = 
    if t1 == t2 then
        Right res
    else 
        Left $ UnmatchedTypes ("Tipo " ++ show t1 ++ " não se encaixa com " ++ show t2)

data TypeError = UseOfMovedValue 
    | UnmatchedTypes String
    | Unimplemented 
    | MissingType 
    | BareLoc 
    | VarNotFound 
    | EndOfScopeWithLinVar 
    | InvalidAliasAcess Env
    deriving (Show)

type Reg = Int

type TypeEnv = Map.Map VarName Tipo
-- fst outlives snd
type RegEnv = Set.Set (Reg, Reg)
type LoanEnv = Map.Map Reg VarName

data Env = EnvT {
    gamma :: TypeEnv,
    used :: TypeEnv,
    regions :: RegEnv,
    loans :: LoanEnv,
    ident :: Reg
} deriving (Show)


-- repairU: junta o gamma e o used
repairU :: Env -> Env
repairU EnvT { gamma, used, regions, loans, ident } = EnvT { gamma = Map.union gamma used, used = Map.empty, regions, loans, ident }

-- discard: discarta o used
discardU :: Env -> Env
discardU EnvT { gamma, regions, loans, ident } = EnvT { gamma, used = Map.empty, regions, loans, ident }

-- checa se todos os elementos em gamma são não lineares
nonlinear :: Map.Map VarName Tipo -> Bool
nonlinear gamma = not $ any (isLinear . snd) (Map.toList gamma)

getVar :: VarName -> Env -> Either TypeError Tipo
getVar name EnvT { gamma } = maybe (Left VarNotFound) Right (Map.lookup name gamma)
-- useVar: checa se gamma tem x, retornando o tipo ou error
-- isso move x de gamma para used
-- TODO: matar aqui as loans associadas a variável?
useVar :: VarName -> Env -> Either TypeError (Env, Tipo) 
useVar name env = do
    tipo <- getVar name env
    let EnvT { gamma, used, regions, ident } = env
    let newLoans = loans $ removeLoan name env
    let newGamma = Map.delete name gamma
    let newUsed = Map.insert name tipo used
    Right (EnvT { gamma = newGamma, used = newUsed, regions, loans = newLoans, ident }, tipo)


isVarLin :: VarName -> Env -> Either TypeError Bool
isVarLin name env = isLinear <$> getVar name env

-- add: adiciona x de tipo T em gamma
addVar :: VarName -> Tipo -> Env -> Env
addVar name t EnvT { gamma, used, loans, regions, ident } = EnvT { 
    gamma = Map.insert name t gamma, 
    used, loans, regions, ident 
}

getLabel :: Env -> (Env, Reg)
getLabel EnvT { gamma, used, regions, loans, ident } = (EnvT { gamma, used, regions, loans, ident = ident + 1 }, ident)

-- add region relation
-- remove region relation is a form of scope ending,
-- então não é necessário adcionar a operação, só restaurar o env, neh?
addRegRelation :: (Reg, Reg) -> Env -> Env
addRegRelation rel EnvT { gamma, used, regions, loans, ident } =
    let transRegions = transitive $ Set.insert rel regions in
    EnvT { 
        gamma, 
        used, 
        regions = transRegions, 
        loans = propateLoans transRegions loans, 
        ident
    } 

transitive :: RegEnv -> RegEnv
-- for every pair (r1 : r2)
-- if there is *another* pair in the list, such as (r0 : r1),
-- then add (r0 : r2). repeat until the set no longer grows
transitive regEnv = 
    let size = Set.size regEnv in
    let regEnv' = foldMap gatter regEnv in
    if Set.size regEnv' > size then transitive regEnv' else regEnv'
    where
        gatter (r1, r2) = Set.foldr' (
            \(r0', r1') acc -> 
                if r1' == r1 then 
                    Set.insert (r0', r2) acc 
                else 
                    acc
            ) 
            regEnv regEnv

-- quando adiciona um emprestimo, propaga para todas as regiões na relação r : r'
-- então quando adiciona (0', x) com (0' : 1'), também adiciona (1', x)
-- TODO: verificar o caso ex3 funciona nessa relação
addLoan :: Reg -> VarName -> Env -> Env 
addLoan r name EnvT { gamma, used, regions, loans, ident } = 
    let regs = Set.filter ((== r) . fst) regions in
    let derivedLoans = Set.foldr (\(_, r1) -> Map.insert r1 name) loans regs in
    --error (show regions ++ "\n" ++ show regs ++ "\n" ++ show derivedLoans)
    EnvT { 
        gamma, 
        used, 
        regions, 
        loans = Map.insert r name derivedLoans, 
        ident
    } 

propateLoans :: RegEnv -> LoanEnv -> LoanEnv
propateLoans regs loans = Map.foldrWithKey pass loans loans
    where
        pass regKey var loansAcc = Set.foldr 
            (\(r1, r2) acc -> if regKey == r1 then Map.insert r2 var acc else acc) loansAcc regs


removeLoan :: VarName -> Env -> Env 
removeLoan name EnvT { gamma, used, regions, loans, ident } = 
    EnvT { gamma, used, regions, loans = Map.filter (/= name) loans, ident} 
-- TODO: validar com casos de uso a implementação
-- um acesso é válido se o empréstimo existe para a região
-- isso pode ser invalidado caso a região que fez o empréstimo tenha saido de escopo
-- ou se o elemento emprestado tenha sido manualmente removida (delete)
isAcessValid :: Reg -> Env -> Bool
isAcessValid r EnvT { loans } = Maybe.isJust $ Map.lookup r loans 


check :: (Env, Exp) -> Either TypeError (Env, Tipo)
-- rule for var
-- if var is non linear, just get the type
-- else get the type and move the var to the used space
-- TODO: remover x do set de loans aqui ou dps, idk?
check (env, Var x) = do
    isLin <- isVarLin x env 
    if isLin then 
        useVar x env 
    else do
        tipo <- getVar x env 
        case tipo of
            Fn _ _ -> Left $ UnmatchedTypes "Nome de variável não pode ser usada como variável"
            _ -> Right (env, tipo)

-- NOTE: mutate env with the lifetime stuff
check (env, Ref name) = do 
    t <- getVar name env
    case t of
        Fn _ _ -> Left $ UnmatchedTypes "Nome de variável não pode ser usada como variável"
        _ -> do 
            let (env', reg) = getLabel env
            Right (env', AliasT t reg)
         

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
                        AliasT t' _ | t == t' -> Right Num
                        rest -> Left $ UnmatchedTypes ("Em Comparação " ++ show rest ++ " não encaixa com @" ++ show t ++ "'" ++ show reg)
                    rest -> Left $ UnmatchedTypes ("Comparação não está definido para tipo " ++ show rest)
            | otherwise = -- exclusivamente Mult
                if t1 == Num && t2 == Num then 
                    Right Num
                else 
                    Left $ UnmatchedTypes ("Comparação não está definido para os tipos " ++ show t1 ++ ", " ++ show t2)

-- G(x), U() ->  x 		   -> G(); U(x)
-- G(x), U() ->  f(x) 	   -> G(); U()
-- G(x), U() ->  f(x) += n -> G(); U()
check (env, Inc expr1 expr2) = do
    (env', t) <- check (env, expr1)
    if isPtr t then do
        (env2, t2) <- check (repairU env', expr2)
        match t2 Num (discardU env2, unit)
    else
        Left $ UnmatchedTypes ("Operador da esquerda deve ser um ponteiro, o atual é " ++ show t)

check (env, Dec expr1 expr2) = do
    (env', t) <- check (env, expr1)
    if isPtr t then do
        (env2, t2) <- check (repairU env', expr2)
        match t2 Num (discardU env2, unit)
    else
        Left $ UnmatchedTypes ("Operador da esquerda deve ser um ponteiro, o atual é " ++ show t)

check (env, Not expr) = do
    (env', t) <- check (env, expr)
    case t of 
        Num -> Right (discardU env', Num)
        AliasT _ _ -> Right (discardU env', Num)
        _ -> Left $ UnmatchedTypes ("Operador not só opera sobre ponteiros e aliases, o atual é " ++ show t)


-- deref *T -> T if T is nonlinear
-- TODO: validate the deref on the alias
check (env, Deref expr) = do 
    (env', t) <- check (env, expr)
    case t of 
        Ptr innerT | not $ isLinear innerT -> Right (repairU env', innerT)
        AliasT innerT reg | not (isLinear innerT) -> 
            if isAcessValid reg env' then 
                Right (repairU env', innerT) 
            else 
                Left (InvalidAliasAcess env)
        _ -> Left $ UnmatchedTypes ("Só pode-se desreferenciar ponteiros para valores não lineares. O atual é " ++ show t)

-- get alias of ptr type
check (env, Alias name) = do
    t <- getVar name env
    let (env', ident) = getLabel env
    case t of
        Ptr t' -> Right (addLoan ident name env', AliasT t' ident)
        _ -> Left $ UnmatchedTypes ("Só pode-se fazer alias de ponteiros. O atual é " ++ show t)

check (env, AliasDeref name) = do
    t <- getVar name env
    case t of
        AliasT t' reg -> case t' of 
            Ptr _ -> Right (addLoan reg name env, AliasT t' reg)
            _ -> Left $ UnmatchedTypes ("Só pode-se fazer alias* de @*T'reg. O tipo interno atual é " ++ show t)
        _ -> Left $ UnmatchedTypes ("O tipo da variável para alias* deve ser AliasT. O atual é " ++ show t)

check (env, Comp expr1 expr2) = do
    (env', _) <- check (env, expr1)
    check (discardU env', expr2)

-- env'/env são as variáveis declaradas em env' que ainda estão vivas
-- se todas elas nesse conjunto são não lineares
-- retorna env `intersect` env' -> 
-- isso resulta nas vars que existiam em env menos as vars de env' e os lineares consumidos em env'
-- G |- e : T | G'    nonlinear G'\G 
------------------------------------
-- G |- { e } : T | G `intersect` G' 
check (env, Scope expr) = do
    (env', t) <- check (env, expr)
    let locals = (Map.difference `on` gamma) env' env
    if nonlinear locals then 
        Right (EnvT { 
            gamma = (Map.intersection `on` gamma) env env',
            used = Map.empty,
            regions = regions env,
            -- TODO: para cada variável que saiu de escopo, remover a loan associada
            loans = Map.filter (`Map.notMember` locals) (loans env'), 
            ident = ident env'
        }, t)
    else
        Left EndOfScopeWithLinVar

check (env, New t expr) = do
    (env', t1) <- check (env, expr)
    match t1 Num (discardU env', Ptr t)

-- TODO: remover a loan da lista de loans
check (env, Delete expr1 expr2) = do
    (env', _) <- check (env, expr1)
    (env'', t2) <- check (discardU env', expr2)
    match t2 Num (discardU env'', unit)

--NOTE: usar o tipo da expressão?
check (env, LetVar name t expr) = do
    (env', t') <- check (env, expr)
    case t of
        AliasT innerT lr ->
            case t' of 
                AliasT innerT' rr | innerT == innerT' -> 
                    Right (addRegRelation (rr, lr) (addVar name t (discardU env')), unit)
                _ -> Left $ UnmatchedTypes "Lado direito da declaração deve ser um Alias T"
        _ -> match t t' (addVar name t (discardU env'), unit)

-- TODO: discartar a relação que existia para a região do lado esquerdo (lr) (será que é necessário na real?)
check (env, Assign expr1 expr2) = do
    (env', t) <- check (env, expr1)
    (env'', t') <- check (discardU env', expr2)
    if  not (isLinear t) && not (isLinear t') then 
        case t of
            AliasT innerT lr ->
                case t' of 
                    AliasT innerT' rr | innerT == innerT' -> 
                        Right (addRegRelation (rr, lr) (discardU env''), unit)
                    _ -> Left $ UnmatchedTypes "Lado direito da declaração deve ser um Alias T"
            _ -> match t t' (discardU env'', unit)
    else
        Left $ UnmatchedTypes ("Os dois tipos da atribuição devem ser não lineares. Os atuais são " ++ show t ++ ", " ++ show t')

-- NOTE: validar o uso de tipos lineares
check (env, Swap expr1 expr2) = do
    (env', t) <- check (env, expr1)
    (env'', t') <- check (repairU env', expr2)
    case t of
        AliasT innerT lr ->
            case t' of 
                AliasT innerT' rr | innerT == innerT' -> 
                    Right (addRegRelation (lr, rr) $ addRegRelation (rr, lr) (repairU env''), unit)
                _ -> Left $ UnmatchedTypes "Lado direito da declaração deve ser um Alias T"
        _ -> match t t' (repairU env'', unit)

check (env, SwapDeref expr1 expr2) = do
    (env', t) <- check (env, expr1)
    (env'', t') <- check (repairU env', expr2)
    tOfPtr <- innerOfPtr t'
    case t of 
        AliasT innerT lr ->
            case tOfPtr of
                AliasT innerT' rr | innerT == innerT' -> 
                    Right (addRegRelation (lr, rr) $ addRegRelation (rr, lr) (repairU env''), unit)
                _ -> Left $ UnmatchedTypes "Lado direito interno da declaração deve ser um Alias T"
        _ -> match t t' (repairU env'', unit)
    where
        innerOfPtr = \case
            AliasT t _ -> Right t
            Ptr t -> Right t
            t' -> Left $ UnmatchedTypes ("Elemento do lado direito deve ser um ponteiro para um ponteiro. Atual é " ++ show t')


-- NOTE: imagina-se que usando o Scope ali ele elimina as lineares e faz o check automático
check (env, If expr1 expr2 expr3) = do 
    (env', t) <- check (env, expr1) 
    if t == Num then do
        (env2, t2) <- check (env', Scope expr2) 
        (env3, t3) <- check (env', Scope expr3) 
        match t2 t3 (EnvT {
            gamma = (Map.intersection `on` gamma) env2 env3,
            used = Map.empty,
            regions = regions env,
            loans = (Map.intersection  `on` loans) env2 env3,
            ident = max (ident env2) (ident env3)
        }, t3)
    else
        Left $ UnmatchedTypes ("Tipo da expressão de comparação deve ser inteiro. Atual é " ++ show t)

-- retornar unit
check (env, While expr1 expr2) = do 
    (env', t) <- check (env, expr1) 
    if t == Num then
        -- apply to the right side of either (<$>)
        -- replace the snd element with unit [] (<$)
        -- isso pois funtores são aplicados ao segundo elemento das tuplas
        (<$) unit <$> check (env', Scope expr2)
    else
        Left $ UnmatchedTypes ("Tipo da expressão de comparação deve ser inteiro. Atual é " ++ show t)

check (env, CallFunc name args) = do
    tipo <- getVar name env
    case tipo of
        Fn tArgs ret -> (,ret) <$> foldr pass (Right env) (zip args tArgs)
        rest -> Left $ UnmatchedTypes ("O nome da função não está associado a uma função. Esta associado a " ++ show rest)
    where 
        pass :: (Exp, Tipo) -> Either TypeError Env -> Either TypeError Env
        pass (e, tipo) (Right envAcc) = do
            (envAcc', checkedTipo) <- check (envAcc, e)
            case checkedTipo of
                AliasT innerT lr ->
                    case tipo of 
                        AliasT innerT' rr | innerT == innerT' -> 
                            Right (addRegRelation (rr, lr) (discardU envAcc'))
                        _ -> Left $ UnmatchedTypes "Argumento da declaração deve ser um Alias T"
                _ -> match checkedTipo tipo (discardU envAcc')
        pass _ (Left err) = Left err

check (env, Stop) = Right (env, unit)

check (env, NullPtr tipo) = Right (env, Ptr tipo)
check (env, NullAlias tipo) = Right (env, AliasT tipo 0)


checkFn :: (Env, Function) -> Either TypeError (Env, Tipo)
checkFn (env, DeclFunc name args retT body nextFn) = do
    -- adiciona o tipo da função na lista antes de validar, pois o corpo pode chamar a função recursivamente
    (env', t) <- check (addVar name (Fn (map snd args) retT) env, body)
    if t == retT then 
        checkFn (env', nextFn) 
    else
        Left $ UnmatchedTypes ("Tipo de retorno não é igual ao tipo do corpo. Retorno é " ++ show retT ++", mas o corpo é " ++ show t)

checkFn (env, Main body) = check (env, Scope body)


checkProgram :: Ast -> Either TypeError Tipo
checkProgram ast = snd <$> checkFn (
    EnvT { 
        gamma = Map.empty, 
        used = Map.empty, 
        regions = Set.empty, 
        loans = Map.empty, 
        ident = 10000
    }, ast)

(.>) :: Exp -> Exp -> Exp
(.>) = Comp

infixr 8 .> 


ex1 :: Function
ex1 = Main (
    LetVar "y" (AliasT Num 1) (NullAlias Num) .>
    Scope (
        LetVar "x" (Ptr Num) (New Num (Value (Number 1))) .>
        Assign (Var "y") (Alias "x") .>
        Delete (Var "x") (Value (Number 1))
    ) .>
    Deref (Var "y")
    )

ex3 :: Function
ex3 = Main (
    LetVar "y" (AliasT Num 1) (NullAlias Num) .>
    LetVar "x" (Ptr Num) (New Num (Value (Number 1))) .>
    Scope (
        Assign (Var "y") (Alias "x")
    ) .>
    Deref (Var "y") .>
    Delete (Var "x") (Value (Number 1))
    )

testMain :: IO ()
testMain = do
    print ex3
    print $ checkProgram ex3