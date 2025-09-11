{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module TypeCheck where


import PclFront

import Data.Function

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (forM_)

match :: (Show a, Eq a) => a -> a -> b -> Either TypeError b
match t1 t2 res = 
    if t1 == t2 then
        Right res
    else 
        Left $ UnmatchedTypes ("Tipo " ++ show t1 ++ " não se encaixa com " ++ show t2)

data TypeError = UseOfMovedValue 
    | UnmatchedTypes String
    | BareLoc 
    | VarNotFound 
    | EndOfScopeWithLinVar 
    | InvalidAliasAcess Env
    | InvalidAliasDeclaration Env
    deriving (Show)

instance Eq TypeError where
    (==) err1 err2 = case err1 of
        UseOfMovedValue -> case err2 of UseOfMovedValue -> True; _ -> False
        UnmatchedTypes _ -> case err2 of UnmatchedTypes _ -> True; _ -> False
        BareLoc -> case err2 of BareLoc -> True; _ -> False
        VarNotFound -> case err2 of VarNotFound -> True; _ -> False
        EndOfScopeWithLinVar -> case err2 of EndOfScopeWithLinVar -> True; _ -> False
        InvalidAliasAcess _ -> case err2 of InvalidAliasAcess _ -> True; _ -> False
        InvalidAliasDeclaration _ -> case err2 of InvalidAliasDeclaration _ -> True; _ -> False

type TypeEnv = Map.Map VarName Tipo
-- fst outlives snd
type RegEnv = Set.Set (Reg, Reg)
type LoanEnv = Set.Set (Reg, VarName)

data Env = EnvT {
    gamma :: TypeEnv,
    used :: TypeEnv,
    regions :: RegEnv,
    loans :: LoanEnv,
    ident :: Reg
} deriving (Show)

emptyEnv :: Env
emptyEnv = EnvT {
    gamma = Map.empty,
    used = Map.empty,
    regions = Set.empty,
    loans = Set.empty,
    ident = 10000
}


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

combineAlias :: Tipo -> Tipo -> [(Reg, Reg)]
combineAlias t1 t2 = case (t1, t2) of
    (AliasT innerT1 r1, AliasT innerT2 r2) -> (r1, r2) : (r2, r1) : combineAlias innerT1 innerT2
    _ -> []

-- if r2 : r1, ou se r1 é outlived by r2
-- that means, find in regions the pair (r2, r1)
isOnlyOutLivedBy :: Reg -> Reg -> Env -> Bool
isOnlyOutLivedBy r1 r2 EnvT { regions } = 
    -- pega todos os Rs em que Rs : r1,
    -- junto da identidade (r1, r1)
    let regionsThatOutliveR1 = Set.filter ((== r1) . snd) regions in
    if Set.size regionsThatOutliveR1 == 0 then -- se ninguem outlives r1, então r1 tem que ser igual a r2
        r1 == r2
    else
    -- se algum Rs não é outlived for r2, (r2 : Rs) 
    -- então r1 não é apenas outlived por r2, ou mlr, não é ultimamente outlived by r2
    -- all regions rl in [(rl, rr)] need to be outlived by r2
    -- error $ show regionsThatOutliveR1
    -- all (\(rl, _) -> Set.member (r2, rl) regions) regionsThatOutliveR1
    all (\(rl, _) -> 
        (
            Set.member (r2, rl) regions && 
            Set.notMember (rl, r2) regions
        ) || r2 == rl
    ) regionsThatOutliveR1


-- quando adiciona um emprestimo, propaga para todas as regiões na relação r : r'
-- então quando adiciona (0', x) com (0' : 1'), também adiciona (1', x)
-- TODO: verificar o caso ex3 funciona nessa relação
addLoan :: Reg -> VarName -> Env -> Env 
addLoan r name EnvT { gamma, used, regions, loans, ident } = 
    let regs = Set.filter ((== r) . fst) regions in
    let derivedLoans = Set.foldr (\(_, r1) -> Set.insert (r1, name)) loans regs in
    --error (show regions ++ "\n" ++ show regs ++ "\n" ++ show derivedLoans)
    EnvT { 
        gamma, 
        used, 
        regions, 
        loans = Set.insert (r, name) derivedLoans, 
        ident
    } 

propateLoans :: RegEnv -> LoanEnv -> LoanEnv
propateLoans regs loans = Set.foldr pass loans loans
    where
        pass (reg, var) loansAcc = Set.foldr 
            (\(r1, r2) acc -> if reg == r1 then Set.insert (r2, var) acc else acc) loansAcc regs


removeLoan :: VarName -> Env -> Env 
removeLoan name EnvT { gamma, used, regions, loans, ident } = 
    -- get all loans for name
    let relatedLoans = Set.filter (\(_, n) -> n == name) loans in
    EnvT { 
        gamma, 
        used, 
        regions, 
        -- aqui pega todos os emprétimos associados a name
        -- se uma região da lista de loans é associada a name, remove ela
        loans = Set.filter (\(r, _) -> all (\(r', _) -> r /= r') relatedLoans) loans, 
        ident
    } 
-- TODO: validar com casos de uso a implementação
-- um acesso é válido se o empréstimo existe para a região
-- isso pode ser invalidado caso a região que fez o empréstimo tenha saido de escopo
-- ou se o elemento emprestado tenha sido manualmente removida (delete)
isAcessValid :: Reg -> Env -> Bool
isAcessValid r EnvT { loans } = Set.size (Set.filter ((== r) . fst) loans) > 0

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
            Right (addLoan reg name env', AliasT t reg)
         

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
check (env, Inc name expr) = do
    t <- getVar name env
    if isPtr t then do
        (env2, t2) <- check (env, expr)
        match t2 Num (discardU env2, unit)
    else
        Left $ UnmatchedTypes ("Operador da esquerda deve ser um ponteiro, o atual é " ++ show t)

check (env, Dec name expr) = do
    t <- getVar name env
    if isPtr t then do
        (env2, t2) <- check (env, expr)
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
    --error $ show env
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
            loans = loans $ foldr (removeLoan . fst) env' (Map.toList locals), 
            ident = ident env'
        }, t)
    else
        Left EndOfScopeWithLinVar

-- TODO: mudar o type check
check (env, New name t expr) = do
    (env', t1) <- check (env, expr)
    case t of 
        Ptr _ -> match t1 Num (addVar name t (discardU env'), Ptr t)
        _ -> Left $ UnmatchedTypes ("O tipo de retorno do new deve ser um tipo * ponteiro, o atual é: " ++ show t)

-- caso do delete com variável, 
-- remove a loan associada da lista de loans
check (env, Delete name expr2) = do
    (env', t) <- useVar name env
    case t of 
        Ptr _ -> do 
            (env'', t2) <- check (removeLoan name env', expr2)
            match t2 Num (discardU env'', unit)
        rest -> Left $ UnmatchedTypes ("Primeiro argumento de Delete deve ser um pointeiro. Atual é " ++ show rest)

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

-- NOTE: discartar a relação que existia para a região do lado esquerdo (lr) (será que é necessário na real?)
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
check (env, Swap name1 name2) = do
    t <- getVar name1 env
    t' <- getVar name2 env
    case t of
        AliasT innerT lr ->
            case t' of 
                AliasT innerT' rr | innerT == innerT' -> 
                    Right (addRegRelation (lr, rr) $ addRegRelation (rr, lr) (repairU env), unit)
                _ -> Left $ UnmatchedTypes "Lado direito da declaração deve ser um Alias T"
        _ -> match t t' (repairU env, unit)

check (env, SwapDeref name1 name2) = do
    t <- getVar name1 env
    t' <- getVar name2 env
    tOfPtr <- innerOfPtr t'
    case t of 
        AliasT innerT lr ->
            case tOfPtr of
                AliasT innerT' rr | innerT == innerT' -> 
                    Right (addRegRelation (lr, rr) $ addRegRelation (rr, lr) (repairU env), unit)
                _ -> Left $ UnmatchedTypes "Lado direito interno da declaração deve ser um Alias T"
        _ -> match t t' (repairU env, unit)
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
        let newRegs = combineAlias t2 t3
        -- error $ show env2 ++ "\n" ++ show env3
        match t2 t3 (EnvT {
            gamma = (Map.intersection `on` gamma) env2 env3,
            used = Map.empty,
            regions = regions $ foldr addRegRelation env newRegs,
            loans = (Set.union `on` loans) env2 env3,
            ident = max (ident env2) (ident env3)
        }, t3)
    else
        Left $ UnmatchedTypes ("Tipo da expressão de comparação deve ser inteiro. Atual é " ++ show t)

{- NOTE: não foi fazer while agr, muito brain pra fazer funcionar, recursion it is bb
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
-}

-- TODO: mudar o type check
check (env, CallFunc name1 t name2 args) = do
    tipo <- getVar name2 env
    case tipo of
        Fn tArgs retT -> do
            resultEnv <- foldr pass (Right env) (zip args tArgs)
            case t of 
                AliasT innerT lr ->
                    case retT of
                        AliasT innerT' rr | innerT == innerT' -> 
                            Right (addVar name1 t $ addRegRelation (lr, rr) $ addRegRelation (rr, lr) resultEnv, unit)
                        _ -> Left $ UnmatchedTypes "Lado direito interno da declaração deve ser um Alias T"
                _ -> match t retT (resultEnv, unit)
        rest -> Left $ UnmatchedTypes ("O nome da função não está associado a uma função. Esta associado a " ++ show rest)
    where 
        pass :: (Exp, Tipo) -> Either TypeError Env -> Either TypeError Env
        pass (e, ltipo) (Right envAcc) = do
            (envAcc', rtipo) <- check (envAcc, e)
            case rtipo of
                AliasT innerT rr ->
                    case ltipo of 
                        AliasT innerT' lr | innerT == innerT' -> 
                            Right (addRegRelation (rr, lr) (discardU envAcc'))
                        _ -> Left $ UnmatchedTypes "Argumento da declaração deve ser um Alias T"
                _ -> match rtipo ltipo (discardU envAcc')
        pass _ (Left err) = Left err

check (env, Stop) = Right (env, unit)

check (env, NullPtr tipo) = Right (env, Ptr tipo)
check (env, NullAlias tipo) = Right (env, AliasT tipo 0)


checkFn :: (Env, Function) -> Either TypeError (Env, Tipo)
checkFn (env, DeclFunc name args retT body nextFn) = do
    -- adiciona o tipo da função na lista antes de validar, pois o corpo pode chamar a função recursivamente
    -- e adiciona os argumentos
    (env', t) <- check (foldr (uncurry addVar) (addVar name (Fn (map snd args) retT) env) args, body)
    -- Change the below equality, it has to check for something more on this on the case of Alias

    case retT of
        AliasT innerRetT retReg -> 
            case t of 
                AliasT innerT reg | innerRetT == innerT -> 
                    -- NOTE: um alias de retorno com região r é válido 
                    -- se apenas a região retR de retorno anotado outlives it, 
                    -- na forma retR : r, ou se eles são iguais
                    if isOnlyOutLivedBy reg retReg env' then
                        --error $ show env' ++ "("++ show retReg ++", " ++ show reg ++")"
                        checkFn (addVar name (Fn (map snd args) retT) env, nextFn)
                    else
                        Left $ InvalidAliasDeclaration env'
                _ -> Left $ UnmatchedTypes ("Tipo de retorno esperado é " ++ show retT ++ ". O atual é " ++ show t)
        _ | t == retT -> checkFn (addVar name (Fn (map snd args) retT) env, nextFn)
        _ -> Left $ UnmatchedTypes ("Tipo de retorno não é igual ao tipo do corpo. Retorno é " ++ show retT ++", mas o corpo é " ++ show t)

checkFn (env, Main body) = check (env, Scope body)


checkProgram :: Ast -> Either TypeError (Env, Tipo)
checkProgram ast = checkFn (emptyEnv, ast)

(.>) :: Exp -> Exp -> Exp
(.>) = Comp

infixr 8 .> 


ex1 :: Function
ex1 = Main (
    LetVar "y" (AliasT Num 1) (NullAlias Num) .>
    Scope (
        New "x" (Ptr Num)  (Value (Number 1)) .>
        -- @int'1 alias'2 *T    
        -- Regions (2 : 1), Loans (2, x), (1, x)
        Assign (Var "y") (Alias "x") .>
        --
        Delete "x" (Value (Number 1))
    ) .>
    Deref (Var "y")
    )

ex2 :: Function
ex2 = Main (
    LetVar "y" (AliasT Num 1) (NullAlias Num) .>
    New "x" (Ptr Num) (Value (Number 1)) .>
    New "z" (Ptr Num) (Value (Number 1)) .>
    If (Value (Number 1)) 
        -- (2x : 1), (x, 2x), (x, 1)
        (Assign (Var "y") (Alias "x")) 
        -- (2z : 1), (z, 2z), (z, 1)
        (Assign (Var "y") (Alias "z")) 
    .>
    -- (x, 1), (z, 1)
    Deref (Var "y") .>
    Delete "x" (Value (Number 1)) .>
    Delete "z" (Value (Number 1))
    )

ex3 :: Function
ex3 = Main (
    LetVar "y" (AliasT Num 1) (NullAlias Num) .>
    New "x" (Ptr Num) (Value (Number 1)) .>
    Scope (
        Assign (Var "y") (Alias "x")
    ) .>
    Deref (Var "y") .>
    Delete "x" (Value (Number 1))
    )

ex4 :: Function
ex4 = Main (
    LetVar "y" (AliasT Num 1) (NullAlias Num) .>
    New "x" (Ptr Num) (Value (Number 1)) .>
    New "z" (Ptr Num) (Value (Number 1)) .>
    If (Value (Number 1)) (Assign (Var "y") (Alias "x")) (Assign (Var "y") (Alias "z")) .>
    Deref (Var "y") .>
    Delete "x" (Value (Number 1)) .>
    -- deref desse y é unsafe, talvez o if tomou a esquerda, então deve falhar
    Deref (Var "y") .>
    Delete "z" (Value (Number 1))
    )

ex5 :: Function
ex5 = DeclFunc "comp" [("a", AliasT Num 10), ("b", AliasT Num 10)] (AliasT Num 10) (Var "a") $
    Main (
        New "x" (Ptr Num) (Value (Number 1)) .>
        New "z" (Ptr Num) (Value (Number 1)) .>
        CallFunc "res" (AliasT Num 55) "comp" [Alias "x", Alias "z"] .>
        Deref (Var "res") .>
        Delete "x" (Value (Number 1)) .>
        Delete "z" (Value (Number 1))
    )
-- quando deleta um dos elementos associados
ex6 :: Function
ex6 = DeclFunc "comp" [("a", AliasT Num 10), ("b", AliasT Num 10)] (AliasT Num 10) (Var "a") $
    Main (
        New "x" (Ptr Num) (Value (Number 1)) .>
        New "z" (Ptr Num) (Value (Number 1)) .>
        CallFunc "res" (AliasT Num 55) "comp" [Alias "x", Alias "z"] .>
        Deref (Var "res") .>
        Delete "x" (Value (Number 1)) .>
        Deref (Var "res") .> -- gera erro aqui
        Delete "z" (Value (Number 1))
    )

-- quando o escopo dos elementos não bate, ig
{-
f(x: @T'b, y: @T'b) -> @T'b
...
let x: T;
let z: @T'a;
    {
        let y: T;
        z := f(&x, &y)
    }
*z <- erro, um,
-}
ex7 :: Function
ex7 = DeclFunc "comp" [("a", AliasT Num 10), ("b", AliasT Num 10)] (AliasT Num 10) 
    (If (Value (Number 0)) (Var "a") (Var "b")) 
    $
    Main (
        LetVar "x" Num (Value (Number 1)) .>
        --LetVar "x" (Ptr Num) (New Num (Value (Number 1))) .>
        LetVar "res" (AliasT Num 55) (NullAlias Num) .>
        Scope (
            LetVar "z" Num (Value (Number 1)) .>
            --LetVar "z" (Ptr Num) (New Num (Value (Number 1))) .>
            CallFunc "fres" (AliasT Num 56) "comp" [Ref "x", Ref "z"] .> 
            Assign (Var "res") (Var "fres") 
            -- Delete (Var "z") (Value $ Number 1)
        ) .>
        Deref (Var "res")
    )
-- essa versão deve funcionar
ex7_1 :: Function
ex7_1 = DeclFunc "comp" [("a", AliasT Num 10), ("b", AliasT Num 10)] (AliasT Num 10) 
    (If (Value (Number 0)) (Var "a") (Var "b")) 
    $
    Main (
        LetVar "x" Num (Value (Number 1)) .>
        LetVar "res" (AliasT Num 55) (NullAlias Num) .>
        Scope (
            LetVar "z" Num (Value (Number 1)) .>
            CallFunc "fres" (AliasT Num 56) "comp" [Ref "x", Ref "z"] .> 
            Assign (Var "res") (Var "fres") .>
            Deref (Var "res")
        )
    )

-- quando os tempos de vida associados da função não funcionam, ig
{-
f(x: @T'a, y: @T'b) -> @T'b { <- aqui acho q análise deve apitar na avaliação da função
    if (...) { x } else { y }
}
...
let x: T;
let y: T;
let z: @T'a;
z := f(&x, &y)
*z
-}
ex8 :: Function
ex8 = DeclFunc "comp" [("a", AliasT Num 10), ("b", AliasT Num 11)] (AliasT Num 11) 
    (If (Value (Number 0)) (Var "a") (Var "b")) 
    $
    Main (
        LetVar "x" Num (Value (Number 1)) .>
        LetVar "z" Num (Value (Number 1)) .>
        CallFunc "res" (AliasT Num 55) "comp" [Ref "x", Ref "z"] .>
        Deref (Var "res") 
    )

-- retorno de alias unbounded da função, i.g
{-
f() -> @T'b { <- aqui acho q análise deve apitar na avaliação da função
    let x;
    &x
}
...
let z := f()
*z
-}
ex9 :: Function
ex9 = DeclFunc "f" [] (AliasT Num 10) 
    (LetVar "x" Num (Value (Number 1)) .> Ref "x") 
    $
    Main (
        CallFunc "res" (AliasT Num 55) "f" [] .>
        Deref (Var "res")
    )

ex10 :: Function
ex10 = Main (
    LetVar "y" (AliasT Num 1) (NullAlias Num) .>
    New "x" (Ptr Num) (Value (Number 1)) .>
    LetVar "z" (AliasT Num 2) (Alias "x") .>
    Scope (
        Assign (Var "y") (Var "z")
    ) .>
    Deref (Var "y") .>
    Delete "x" (Value (Number 1))
    )

ex11 :: Function
ex11 = Main (
    LetVar "y" (AliasT Num 1) (NullAlias Num) .>
    New "x" (Ptr Num) (Value (Number 1)) .>
    Scope (
        LetVar "z" (AliasT Num 2) (Alias "x") .>
        Assign (Var "y") (Var "z")
    ) .>
    Deref (Var "y") .>
    Delete "x" (Value (Number 1))
    )

ex12 :: Function
ex12 = Main (
    LetVar "y" (AliasT Num 1) (NullAlias Num) .>
    Scope (
        New "x" (Ptr Num) (Value (Number 1)) .>
        LetVar "z" (AliasT Num 2) (Alias "x") .>
        Assign (Var "y") (Var "z") .> 
        Delete "x" (Value (Number 1))
    ) .>
    Deref (Var "y")
    )


tests :: [(String, Function, Either TypeError Tipo)]
tests = [
    ("1. Deleted Ptr", ex1, Left $ InvalidAliasAcess emptyEnv), 
    ("2. If borrow", ex2, Right Num), 
    ("3. alias in scope", ex3, Right Num), 
    ("4. If borrow when one is deleted", ex4, Left $ InvalidAliasAcess emptyEnv),
    ("5. Base alias in function", ex5, Right Num),
    ("6. Return of function when an associated loan is deleted", ex6, Left $ InvalidAliasAcess emptyEnv),
    ("7. Deref return when scopes dont match", ex7, Left $ InvalidAliasAcess emptyEnv),
    ("8. Return region of function is insuficient", ex8, Left $ InvalidAliasDeclaration emptyEnv),
    ("9. Return of local adress variable", ex9, Left $ InvalidAliasDeclaration emptyEnv),
    ("10. Assign with ref still in scope", ex10, Right Num),
    ("11. Assign with ref still in scope, inside scope", ex11, Right Num),
    ("12. Assign with ref with ptr inside scope", ex12, Left $ InvalidAliasAcess emptyEnv)
    ]

testMain :: IO ()
testMain = forM_ tests testCase
    where
        testCase (name, prog, expect) = do
            putStr name
            case (checkProgram prog, expect) of 
                (Left err, Left expectedErr) | err == expectedErr -> putStr ": OK\n"
                (Left err, Left expectedErr) -> printFailure err expectedErr
                (Right (_, tipo), Left expectedErr) -> printFailure tipo expectedErr
                (Right (_, tipo), Right expectedTipo) | tipo == expectedTipo -> putStr ": OK\n"
                (Right (_, tipo), Right expectedTipo) -> printFailure tipo expectedTipo
                (Left err, Right expectedTipo) -> printFailure err expectedTipo
        printFailure :: (Show a, Show b) => a -> b -> IO ()
        printFailure e1 e2 = print $ "Falha, esperava\n1: " ++ show e2 ++ "\nEncontrou\n2: " ++ show e1