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
    | Binop Exp Binop Exp
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
pilhaGet :: Loc -> Pilha -> Exp

pilhaGet (loc, Pilha) pilha = maybe 
    (error "Local inexistente na memória")
    (\value -> Value (case value of  --se pilhaGetMv retorna Just, aplica essa função
        Ploc ploc -> Loc ploc
        Pnum number -> Number number 
        _ -> error "falha na conversão"
    ))
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

data MemValues = Mloc Loc | Mnum Number | MBot | MNull deriving (Show, Eq)
type Mem = [MemValues]

--- Obtém o valor de memória da memória
memGetMv :: Loc -> Mem -> Maybe MemValues

memGetMv _ [] = Nothing
memGetMv (0, Memoria) (h : t) = Just h
memGetMv (loc, Memoria) (h : t) = memGetMv (loc - 1, Pilha) t

--- Obtém o valor sintático da memória
memGet :: Loc -> Mem -> Exp

memGet loc mem = maybe 
    (error "Local inexistente na memória")
    (\value -> Value (case value of 
        Mloc loc -> Loc loc
        Mnum number -> Number number 
        _ -> error "falha na conversão"
    ))
    (memGetMv loc mem)

--- Coloca o MemValue no Loc da Memória
memInsert :: Loc -> MemValues -> Mem -> Mem

memInsert (0, Memoria) value (_ : t) = value : t 
memInsert (loc, Memoria) value (h : t) = h : memInsert (loc - 1, Memoria) value t 
memInsert _ _ [] = error "Index Out of bounds na memória"


--- Insere n valores de memória apartir de l em mem
memSet :: Loc -> Int -> MemValues -> Mem -> Mem
-- finished
memSet (0, Memoria) 0 _ mem = mem    
-- se chegou no local, começa trocar a cabeça por mv
memSet (0, Memoria) n mv (h : t) = mv : memSet (0, Memoria) (n - 1) mv t 
-- se chegou no local e não tem nada, insere mv no fim
memSet (0, Memoria) n mv [] = mv : memSet (0, Memoria) (n - 1) mv []     
-- se n chegou no local, segue a lista
memSet (loc, Memoria) n mv (h : t) = h : memSet (loc - 1, Memoria) n mv t  
-- se chegou ao fim da lista, mas n no local, insere MNull até chegar no local
memSet (loc, Memoria) n mv [] = MNull : memSet (loc - 1, Memoria) n mv []  

--- encontra Int espaço consecutivos vagos na memória Mem, retornando Loc.
--- a função pode retornar um Loc = size(Mem), nesse caso, 
--- a memória não contém espaço vago no momento e deve ser expandida
loc :: Int -> Mem -> Loc
loc = loc' 0 (1, Memoria)

loc' :: Int -> Loc -> Int -> Mem -> Loc
loc' count (l, Memoria) size m = case memGetMv (l + count, Memoria) m of
    Nothing -> (l, Memoria) -- chegou no fim da memória
    --- se, com uma célula vaga, achou o espaço, retorna l, se não, aumenta count e segue pro próximo
    Just MNull -> if size == count then
        (l, Memoria) else loc' (count + 1) (l, Memoria) size m
    Just _ -> loc' 0 (l + 1, Memoria) size m


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
--- *lp := v
expStep (Assign (Deref (Value (Loc (local, Pilha)))) (Value v), funcs, env, pilha, mem) = 
    let pilha' = pilhaSet (local, Pilha) v pilha in 
    expStep (Value v, funcs, env, pilha', mem)



-- Para a avaliação quando chega em valor
expStep (Value value, funcs, env, pilha, mem) = (Value value, funcs, env, pilha, mem)

