{-# LANGUAGE LambdaCase #-}

import Data.Map

type Number = Int
type VarName = String
type FuncName = String

data Locals = Pilha | Memoria
    deriving (Show, Eq)
type Loc = (Number, Locals)

data Value = Number Number | Loc Loc
    deriving (Show)

data Binop = Add | Sub | Mult | Less | Greater | Equal | And | Or 
    deriving (Show)

data Exp = Var VarName
    | Value Value
    | Binop Binop Exp Exp
    | Not Exp
    | As Exp Locals
    | Deref Exp
    | Ref Exp
    | Comp Exp Exp
    | Scope Exp -- equivalente a {}
    | Scope' Exp -- equivalente a escopo e
    | Malloc Exp 
    | Free Exp Exp 
    | Let VarName Number
    | Assign Exp Exp
    | If Exp Exp Exp
    | While Exp Exp
    | CallFunc FuncName [Exp]
    deriving (Show)

data Function = DeclFunc FuncName [(VarName, Number)] Exp Function | Main Exp
    deriving (Show)


--- Ambiente de nomes
data EnvValues = Frame (Map VarName Loc) | Stop
type Env = [EnvValues]

--- Obter valores do ambiente de Nomes
envGet :: VarName -> Env -> Loc

envGet nome (Stop : env) =  error "Variável inexistente no escopo"
envGet nome [] =  error "Variável inexistente no escopo"
envGet nome (Frame frame : env) = case Data.Map.lookup nome frame of
    (Just loc) -> loc 
    Nothing -> envGet nome env

--- insere valor no ambiente, se já existe, sobrescreve
envSet :: VarName -> Loc -> Env -> Env

envSet nome loc (Stop : env) = error "Topo da pilha corrompido, contendo Stop." 
envSet nome loc [] = error "Topo da pilha corrompido, topo vazio." 
envSet nome loc (Frame frame : env) = Frame (Data.Map.insert nome loc frame) : env


--- Ambiente de Funções
type FuncBody = ([(VarName, Number)], Exp)
type Funcs = Map FuncName FuncBody

--- Obtem o corpo e args da função
funcsGet :: FuncName -> Funcs -> FuncBody

funcsGet nome funcs = case Data.Map.lookup nome funcs of
    (Just body) -> body
    Nothing -> error "Nome de função inexistente no programa"

--- insere o corpo e args da função no ambiente
funcsSet :: FuncName -> FuncBody -> Funcs -> Funcs

funcsSet = Data.Map.insert 


--- Pilha de Valores

data PiValues = Ploc Loc | Pnum Number | Pbot | Pnull | Pstack | Pfunc deriving (Show, Eq)
type Pilha = [PiValues]

--- Obtém o valor da pilha da pilha de valores
pilhaGetMv :: Int -> Pilha -> Maybe PiValues

pilhaGetMv _ [] = Nothing
pilhaGetMv 0 (h : t) = Just h
pilhaGetMv loc (h : t) = pilhaGetMv (loc - 1) t

--- Obtém o valor sintático da pilha de valores
pilhaGet :: Loc -> Pilha -> Value

pilhaGet (loc, Pilha) pilha = maybe 
    (error "Local inexistente na memória")
    (\case --se pilhaGetMv retorna Just, aplica essa função
        Ploc ploc -> Loc ploc
        Pnum number -> Number number 
        _ -> error "falha na conversão"
    )
    (pilhaGetMv loc pilha)

--- Coloca o PiValue no Loc da Pilha
pilhaSetMv :: Number -> PiValues -> Pilha -> Pilha

pilhaSetMv 0 value (_ : t) = value : t
pilhaSetMv loc value (h : t) = h : pilhaSetMv (loc - 1) value t
pilhaSetMv _ _ [] = error "Index Out of bounds na pilha"

--- Coloca o Value no Loc da Pilha
pilhaSet :: Loc -> Value -> Pilha -> Pilha
--- Parece o demônio da babilônia, mas faz a sequência:
--- Checa se o local de inserção é valido (se é bot, num ou local)
--- se é, coloca o valor como valor de pilha
pilhaSet (loc, Pilha) value pilha = 
    let isValid = maybe False (\case 
            Pnum _ -> True
            Ploc _ -> True
            Pbot -> True 
            _ -> False) 
            (pilhaGetMv loc pilha) 
    in if isValid then 
        pilhaSetMv loc (case value of 
            Number number -> Pnum number
            Loc loc -> Ploc loc
        ) pilha
    else
        error "Inserção em local de memória inválido"
    

--- Remove o topo da pilha até o valor value, esse sempre será Pstack ou Pfunc
pilhaPop :: PiValues -> Pilha -> Pilha

pilhaPop value (h : t) = if h /= value then pilhaPop value t else t
pilhaPop _ [] = []


--- Memória

data MemValues = Mloc Loc | Mnum Number | Mbot | Mnull deriving (Show, Eq)
type Mem = [MemValues]

--- Obtém o valor de memória da memória
memGetMv :: Int -> Mem -> Maybe MemValues

memGetMv _ [] = Nothing
memGetMv 0 (h : t) = Just h
memGetMv loc (h : t) = memGetMv (loc - 1) t

--- Obtém o valor sintático da memória
memGet :: Loc -> Mem -> Value

memGet (loc, Memoria) mem = maybe 
    (error "Local inexistente na memória")
    (\case 
        Mloc loc' -> Loc loc'
        Mnum number -> Number number 
        _ -> error "falha na conversão"
    )
    (memGetMv loc mem)

--- Coloca o MemValue no Loc da Memória
memInsertMv :: Int -> MemValues -> Mem -> Mem

memInsertMv 0 value (_ : t) = value : t
memInsertMv loc value (h : t) = h : memInsertMv (loc - 1) value t
memInsertMv _ _ [] = error "Index Out of bounds na memória"

--- Coloca um Value na memória
--- quase que exatamente igual a pilhaSet
memInsert :: Loc -> Value -> Mem -> Mem
memInsert (loc, Pilha) value mem = 
    let isValid = maybe False (\case 
            Mnull -> False
            _ -> True) 
            (memGetMv loc mem) 
    in if isValid then 
        memInsertMv loc (case value of 
            Number number -> Mnum number
            Loc loc -> Mloc loc
        ) mem
    else
        error "Inserção em local de memória inválido"



--- Insere n valores de memória apartir de l em mem
--- loc -> size -> value -> memoria
memSet :: Int -> Int -> MemValues -> Mem -> Mem
-- finished
memSet 0 0 _ mem = mem    
-- se chegou no local, começa trocar a cabeça por mv
memSet 0 n mv (h : t) = mv : memSet 0 (n - 1) mv t 
-- se chegou no local e não tem nada, insere mv no fim
memSet 0 n mv [] = mv : memSet 0 (n - 1) mv []     
-- se n chegou no local, segue a lista
memSet loc n mv (h : t) = h : memSet (loc - 1) n mv t  
-- se chegou ao fim da lista, mas n no local, insere MNull até chegar no local
memSet loc n mv [] = Mnull : memSet (loc - 1) n mv []  

--- encontra Int espaço consecutivos vagos na memória Mem, retornando Loc.
--- a função pode retornar um Loc = size(Mem), nesse caso, 
--- a memória não contém espaço vago no momento e deve ser expandida
findMem :: Int -> Mem -> Loc
findMem = findMem' 0 (1, Memoria)

findMem' :: Int -> Loc -> Int -> Mem -> Loc
findMem' count (l, Memoria) size m = case memGetMv (l + count) m of
    Nothing -> (l, Memoria) -- chegou no fim da memória
    --- se, com uma célula vaga, achou o espaço, retorna l, se não, aumenta count e segue pro próximo
    Just Mnull -> if size == count then
        (l, Memoria) else findMem' (count + 1) (l, Memoria) size m
    Just _ -> findMem' 0 (l + 1, Memoria) size m


--- Função auxiliar para a avaliação das expressões aritméticas e booleanas

binop :: Binop -> Value -> Value -> Value
--- no binop, ele só converte os número e ajeita o tipo de saída
--- nota-se que não existe lp + lm ou vice versa
binop op (Number n1) (Number n2) = Number (binop' op n1 n2)
binop op (Number num) (Loc (loc, Pilha)) = Loc (binop' op num loc, Pilha)
binop op (Loc (loc, Pilha)) (Number num) = Loc (binop' op num loc, Pilha)
binop op (Number num) (Loc (loc, Memoria)) = Loc (binop' op num loc, Memoria)
binop op (Loc (loc, Memoria)) (Number num) = Loc (binop' op num loc, Memoria)

binop' :: Binop -> Number -> Number -> Number

binop' Add n1 n2 = n1 + n2
binop' Sub n1 n2 = n1 - n2
binop' Mult n1 n2 = n1 * n2
binop' Less n1 n2 = if n1 < n2 then 1 else 0
binop' Greater n1 n2 = if n1 > n2 then 1 else 0
binop' Equal n1 n2 = if n1 == n2 then 1 else 0
binop' And n1 n2 = if (n1 /= 0) && (n2 /= 0) then 1 else 0
binop' Or n1 n2 = if (n1 /= 0) || (n2 /= 0) then 1 else 0

--- Converte valor para o seu número base
toNumber :: Value -> Number 
toNumber value = case value of
    Number num -> num
    Loc (loc , _) -> loc


--- Semântica operacional

-- Em haskell, declarações nesse padrão devem ser declaradas
--  do mais específicos ao menos específico.

-- Passa por todas as funções e salva ela em Funcs. Para quando chega na Main
functionStep :: (Function, Funcs, Env, Pilha, Mem) -> (Function, Funcs, Env, Pilha, Mem)

functionStep (DeclFunc funcname args body function, funcs, env, pilha, mem) =
    functionStep (function, funcsSet funcname (args, body) funcs, env, pilha, mem)
functionStep (Main exp, funcs, env, pilha, mem) = (Main exp, funcs, env, pilha, mem)


expStep :: (Exp, Funcs, Env, Pilha, Mem) -> (Exp, Funcs, Env, Pilha, Mem)

--- ====================
--- Regras de composição
--- ====================

--- Compose-combine
expStep (Comp (Value _) exp, funcs, env, pilha, mem) = 
    expStep (exp, funcs, env, pilha, mem)

--- Compose-step
expStep (Comp exp1 exp2, funcs, env, pilha, mem) = 
    let (exp1', _, env', pilha', mem') = expStep (exp1, funcs, env, pilha, mem) in
        expStep (Comp exp1' exp2, funcs, env', pilha', mem')

--- Escopo-init
--- Empilha os elementos nas pilha e muda o código para Scope' 
--- para isso ser feito só no primeiro passo
expStep (Scope exp, funcs, env, pilha, mem) = 
    expStep (Scope' exp, funcs, Frame empty : env, Pstack : pilha, mem)

--- Escopo-pop
--- remove-se o frame do topo do ambiente de nomes e 
--- remove elementos da pilha até o código de controle Pop
expStep (Scope' (Value v), funcs, env, pilha, mem) = 
    let (_ : env') = env in -- desempilha o topo de env
    let pilha' = pilhaPop Pstack pilha in  --- remove valores até Pstack
    expStep (Value v, funcs, env', pilha', mem)

--- Escopo-step
expStep (Scope' exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        expStep (Scope' exp', funcs, env', pilha', mem')


--- ====================
--- Regras de atribuição
--- ====================

--- Let
expStep (Let nome size, funcs, env, pilha, mem) = 
    let topoPilha = length pilha in
    let env' = envSet nome (topoPilha, Pilha) env in -- coloca nome -> local no env de nomes
    let pilha' = replicate size Pbot ++ pilha in -- empilha size Pbot na pilha (tamanho da variável)
    expStep (Value (Loc (topoPilha, Pilha)), funcs, env', pilha', mem)

--- Atribui-deref-pilha
--- *lp := value
expStep (Assign (Deref (Value (Loc (local, Pilha)))) (Value value), funcs, env, pilha, mem) = 
    let pilha' = pilhaSet (local, Pilha) value pilha in 
    expStep (Value value, funcs, env, pilha', mem)

--- Atribui-deref-mem
--- *lm := value
expStep (Assign (Deref (Value (Loc (local, Memoria)))) (Value value), funcs, env, pilha, mem) = 
    let mem' = memInsert (local, Memoria) value mem in 
    expStep (Value value, funcs, env, pilha, mem')

--- Atribui-var
--- x := value
expStep (Assign (Var varName) (Value value), funcs, env, pilha, mem) = 
    let local = envGet varName env in
    let pilha' = pilhaSet local value pilha in 
    expStep (Value value, funcs, env, pilha', mem)

--- Atribui-deref-right-step
expStep (Assign (Deref (Value (Loc loc))) exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        expStep (Assign (Deref (Value (Loc loc))) exp', funcs, env', pilha', mem')

--- Atribui-var-right-step
expStep (Assign (Var varName) exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        expStep (Assign (Var varName) exp', funcs, env', pilha', mem')

--- Atribui-deref-left-step
expStep (Assign (Deref exp1) exp2, funcs, env, pilha, mem) = 
    let (exp1', _, env', pilha', mem') = expStep (exp1, funcs, env, pilha, mem) in
        expStep (Assign (Deref exp1') exp2, funcs, env', pilha', mem')


--- ======================
--- Manipulação de memória
--- ======================

--- Free
expStep (Free (Value (Loc (loc, Memoria))) (Value (Number num)), funcs, env, pilha, mem) = 
    let mem' = memSet loc num Mnull mem in 
    expStep (Value (Number num), funcs, env, pilha, mem')

--- Free-right-step
expStep (Free (Value (Loc (loc, Memoria))) exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        expStep (Free (Value (Loc (loc, Memoria))) exp', funcs, env', pilha', mem')

--- Free-left-step
expStep (Free exp1 exp2, funcs, env, pilha, mem) = 
    let (exp1', _, env', pilha', mem') = expStep (exp1, funcs, env, pilha, mem) in
        expStep (Free exp1' exp2, funcs, env', pilha', mem')

--- Malloc
expStep (Malloc (Value (Number amount)), funcs, env, pilha, mem) = 
    let (loc, Memoria) = findMem amount mem in
    let mem' = memSet loc amount Mbot mem in 
    expStep (Value (Loc (loc, Memoria)), funcs, env, pilha, mem')

--- Malloc-step
expStep (Malloc exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        expStep (Malloc exp', funcs, env', pilha', mem')


--- ==================================
--- Expressões Aritméticas e Booleanas
--- ==================================

--- As
--- Obtém o número interno do valor e torna ele uma localização do tipo local
expStep (As (Value value) local, funcs, env, pilha, mem) = 
    let number = toNumber value
    in expStep (Value (Loc (number, local)), funcs, env, pilha, mem)

--- As-step
expStep (As exp local, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        expStep (As exp' local, funcs, env', pilha', mem')

--- Not
expStep (Not (Value value), funcs, env, pilha, mem) = 
    let result = Number (if toNumber value /= 0 then 0 else 1)
    in expStep (Value result, funcs, env, pilha, mem)

--- Not-step
expStep (Not exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        expStep (Not exp', funcs, env', pilha', mem')

--- Binop
expStep (Binop op (Value v1) (Value v2), funcs, env, pilha, mem) = 
    let result = binop op v1 v2 
    in expStep (Value result, funcs, env, pilha, mem)

--- Binop-rigth-step
expStep (Binop op (Value v) exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        expStep (Binop op (Value v) exp', funcs, env', pilha', mem')

--- Binop-left-step
expStep (Binop op exp1 exp2, funcs, env, pilha, mem) = 
    let (exp1', _, env', pilha', mem') = expStep (exp1, funcs, env, pilha, mem) in
        expStep (Binop op exp1' exp2, funcs, env', pilha', mem')


--- Deref-memoria
expStep (Deref (Value (Loc (local, Memoria))), funcs, env, pilha, mem) = 
    let value = memGet (local, Memoria) mem in 
    expStep (Value value, funcs, env, pilha, mem)

--- Deref-pilha
expStep (Deref (Value (Loc (local, Pilha))), funcs, env, pilha, mem) = 
    let value = pilhaGet (local, Pilha) pilha in 
    expStep (Value value, funcs, env, pilha, mem)

--- Ref
expStep (Ref (Var varName), funcs, env, pilha, mem) = 
    let loc = envGet varName env in 
    expStep (Value (Loc loc), funcs, env, pilha, mem)

--- Var
expStep (Var varName, funcs, env, pilha, mem) = 
    let loc = envGet varName env in 
    let value = pilhaGet loc pilha in
    expStep (Value value, funcs, env, pilha, mem)


--- ==================================
--- Expressões de Controle
--- ==================================

--- While
--- converte While num if 
expStep (While exp1 exp2, funcs, env, pilha, mem) = 
    expStep (If exp1 (Comp exp2 (While exp1 exp2)) (Value (Number 0)), funcs, env, pilha, mem)

--- If
expStep (If (Value cond) exp1 exp2, funcs, env, pilha, mem) = 
    if toNumber cond /= 0 then
        expStep (exp1, funcs, env, pilha, mem) --true
    else
        expStep (exp2, funcs, env, pilha, mem) --false

--- If-step
expStep (If exp1 exp2 exp3, funcs, env, pilha, mem) = 
    let (exp1', _, env', pilha', mem') = expStep (exp1, funcs, env, pilha, mem) in
        expStep (If exp1' exp2 exp3, funcs, env', pilha', mem')


--- ==================================
--- Chamada de função
--- ==================================

expStep (CallFunc funcName args, funcs, env, pilha, mem) = 
    let (declargs, body) = funcsGet funcName funcs in 
        if length declargs /= length args then
            error "Chamada de função com o número errado de argumentos"
        else 
            -- gera uma árvore sintática apartir do zip das listas
            let expTree = Prelude.foldl 
                    -- cada elemento vira um let varName[varSize]; varName := varExp
                    (\acc ((varName, varSize), varExp) -> 
                        Comp (Comp (Let varName varSize) (Assign (Var varName) varExp)) acc) 
                    body 
                    -- usa-se o reverse pois essa árvore cresce da folha a raiz, ou seja, o primeiro
                    -- elemento da lista vai ser o último na árvore
                    (reverse $ zip declargs args) in
            expStep (Scope expTree, funcs, env, pilha, mem)


-- Para a avaliação quando chega em valor
expStep (Value value, funcs, env, pilha, mem) = (Value value, funcs, env, pilha, mem)

