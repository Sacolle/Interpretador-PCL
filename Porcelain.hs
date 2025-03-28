{-# LANGUAGE LambdaCase #-}

module Porcelain where

import Data.Map ( Map, insert, empty, lookup, toList )

type Number = Int
type VarName = String
type FuncName = String

data Locals = Pilha | Memoria
    deriving (Show, Eq)
type Loc = (Number, Locals)

data Value = Number Number | Loc Loc
    deriving (Eq)

instance Show Value where
    show v = case v of
        Number n -> show n
        Loc (loc, local) -> "("++ show loc ++ ", " ++ show local ++ ")"


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


data Exp = Var VarName
    | Value Value
    | Binop Binop Exp Exp
    | Not Exp
    | As Exp Locals
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


instance Show Exp where
    show exp = case exp of
        Var name -> name
        Value value -> show value
        Binop op exp1 exp2 -> show exp1 ++ show op ++ show exp2
        Not exp -> "!" ++ show exp
        As exp local -> show exp ++ " as " ++ show local
        Deref exp -> "*" ++ show exp
        Ref name -> "&" ++ name
        Comp exp1 exp2 -> show exp1 ++ ";\n" ++ show exp2
        Scope exp -> "{ " ++ show exp ++" }" -- equivalente a {}
        Pop exp -> "pop " ++ show exp 
        Malloc exp -> "malloc( " ++ show exp ++ " )" 
        Free exp1 exp2 -> "free( " ++ show exp1 ++ ", " ++ show exp2 ++ " )"
        Let name num -> "let " ++ name ++ "[" ++ show num ++ "]"
        Assign exp1 exp2 -> show exp1 ++ " := " ++ show exp2 
        If exp1 exp2 exp3 -> "if( " ++ show exp1 ++ " )\n" ++ show exp2 ++ "\n" ++ show exp3
        While exp1 exp2 -> "while( " ++ show exp1 ++ " )\n" ++ show exp2
        CallFunc name exps -> name ++ "(" ++ 
            Prelude.drop 2 (Prelude.foldl (\acc exp -> acc ++ ", " ++ show exp) "" exps) 
            ++ ")" 
        Fpop exp -> "Fpop " ++ show exp 


data Function = DeclFunc FuncName [VarName] Exp Function | Main Exp
    deriving (Show)


--- Ambiente de nomes
data EnvValues = Frame (Map VarName Loc) | Stop
type Env = [EnvValues]

instance Show EnvValues where
    show value = case value of 
        Stop -> "Stop"
        Frame map -> "{" ++ Prelude.drop 2 
            (Prelude.foldl 
                (\acc (key, value) -> acc ++ ", " ++ key ++ ":" ++ show value) 
                "" 
                (Data.Map.toList map)) 
            ++ "}"

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
type FuncBody = ([VarName], Exp)
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
--- indexa com (length pilha) o topo e 0 a base.
pilhaGetMv :: Int -> Pilha -> Maybe PiValues

pilhaGetMv loc pilha = pilhaGetMv' loc (length pilha - 1) pilha

pilhaGetMv' :: Int -> Int -> Pilha -> Maybe PiValues

pilhaGetMv' _ _ [] = Nothing
pilhaGetMv' loc size (h : t) = if loc == size then Just h
    else pilhaGetMv' loc (size - 1) t


--- Obtém o valor sintático da pilha de valores
pilhaGet :: Loc -> Pilha -> Value

pilhaGet (loc, Pilha) pilha = maybe 
    (error "Local inexistente na memória")
    (\case --se pilhaGetMv retorna Just, aplica essa função
        Ploc ploc -> Loc ploc
        Pnum number -> Number number 
        _ -> error "falha na conversão de pilha"
    )
    (pilhaGetMv loc pilha)

pilhaGet (_, Memoria) _ = error "chamada da função pilhaGet com um endereço de memória"

--- Coloca o PiValue no Loc da Pilha
pilhaSetMv :: Int -> PiValues -> Pilha -> Pilha

pilhaSetMv loc value pilha = pilhaSetMv' loc (length pilha - 1) value pilha

pilhaSetMv' :: Int -> Int -> PiValues -> Pilha -> Pilha

pilhaSetMv' _ _ _ [] = error "Index Out of bounds na pilha"
pilhaSetMv' loc size value (h : t) = if loc == size then value : t
    else h : pilhaSetMv' loc (size - 1) value t

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

pilhaSet (_, Memoria) _ _= error "chamada da função pilhaSet com um endereço de memória"
    

--- Remove o topo da pilha até o valor value, esse sempre será Pstack ou Pfunc
-- considere value como Pstack
--Topo
-- V
-- 4 : 7 : Pstack : 8 : 0 : nil - não é Pstack, remove e continua
-- 7 : Pstack : 8 : 0 : nil - não é Pstack, remove e continua
-- Pstack : 8 : 0 : nil para e retorna 8 : 0 : nil

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
        _ -> error "falha na conversão de memória"
    )
    (memGetMv loc mem)

memGet (_, Pilha) _= error "chamada da função memGet com um endereço de pilha"

--- Coloca o MemValue no Loc da Memória
memInsertMv :: Int -> MemValues -> Mem -> Mem

memInsertMv 0 value (_ : t) = value : t
memInsertMv loc value (h : t) = h : memInsertMv (loc - 1) value t
memInsertMv _ _ [] = error "Index Out of bounds na memória"

--- Coloca um Value na memória
--- quase que exatamente igual a pilhaSet
memInsert :: Loc -> Value -> Mem -> Mem
memInsert (loc, Memoria) value mem = 
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

memInsert (_, Pilha) _ _= error "chamada da função memInsert com um endereço de pilha"


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
functionStep :: (Function, Funcs) -> (Function, Funcs)

functionStep (DeclFunc funcname args body function, funcs) =
    functionStep (function, funcsSet funcname (args, body) funcs)
functionStep (Main exp, funcs) = (Main exp, funcs)


expStep :: (Exp, Funcs, Env, Pilha, Mem) -> (Exp, Funcs, Env, Pilha, Mem)

--- ====================
--- Regras de composição
--- ====================

--- Compose-combine
expStep (Comp (Value _) exp, funcs, env, pilha, mem) = 
    (exp, funcs, env, pilha, mem)

--- Compose-step
expStep (Comp exp1 exp2, funcs, env, pilha, mem) = 
    let (exp1', _, env', pilha', mem') = expStep (exp1, funcs, env, pilha, mem) in
        (Comp exp1' exp2, funcs, env', pilha', mem')


--- Escopo-init
--- Empilha os elementos nas pilha e muda o código para Scope' 
--- para isso ser feito só no primeiro passo
expStep (Scope exp, funcs, env, pilha, mem) = 
    (Pop exp, funcs, Frame empty : env, Pstack : pilha, mem)

--- Escopo-pop
--- remove-se o frame do topo do ambiente de nomes e 
--- remove elementos da pilha até o código de controle Pop
expStep (Pop (Value v), funcs, env, pilha, mem) = 
    let (_ : env') = env in -- desempilha o topo de env
    let pilha' = pilhaPop Pstack pilha in  --- remove valores até Pstack
    (Value v, funcs, env', pilha', mem)

--- Escopo-step
expStep (Pop exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        (Pop exp', funcs, env', pilha', mem')


--- ====================
--- Regras de atribuição
--- ====================

--- Let
expStep (Let nome size, funcs, env, pilha, mem) = 
    let topoPilha = length pilha in
    let env' = envSet nome (topoPilha, Pilha) env in -- coloca nome -> local no env de nomes
    let pilha' = replicate size Pbot ++ pilha  in -- empilha size Pbot na pilha (tamanho da variável)
    (Value (Loc (topoPilha, Pilha)), funcs, env', pilha', mem)

--- Atribui-deref-pilha
--- *lp := value
expStep (Assign (Deref (Value (Loc (local, Pilha)))) (Value value), funcs, env, pilha, mem) = 
    let pilha' = pilhaSet (local, Pilha) value pilha in 
    (Value value, funcs, env, pilha', mem)

--- Atribui-deref-mem
--- *lm := value
expStep (Assign (Deref (Value (Loc (local, Memoria)))) (Value value), funcs, env, pilha, mem) = 
    let mem' = memInsert (local, Memoria) value mem in 
    (Value value, funcs, env, pilha, mem')

--- Atribui-var
--- x := value
expStep (Assign (Var varName) (Value value), funcs, env, pilha, mem) = 
    let local = envGet varName env in
    let pilha' = pilhaSet local value pilha in 
    (Value value, funcs, env, pilha', mem)

--- Atribui-deref-right-step
expStep (Assign (Deref (Value (Loc loc))) exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        (Assign (Deref (Value (Loc loc))) exp', funcs, env', pilha', mem')

--- Atribui-var-right-step
expStep (Assign (Var varName) exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        (Assign (Var varName) exp', funcs, env', pilha', mem')

--- Atribui-deref-left-step
expStep (Assign (Deref exp1) exp2, funcs, env, pilha, mem) = 
    let (exp1', _, env', pilha', mem') = expStep (exp1, funcs, env, pilha, mem) in
        (Assign (Deref exp1') exp2, funcs, env', pilha', mem')


--- ======================
--- Manipulação de memória
--- ======================

--- Free
expStep (Free (Value (Loc (loc, Memoria))) (Value (Number num)), funcs, env, pilha, mem) = 
    let mem' = memSet loc num Mnull mem in 
    (Value (Number num), funcs, env, pilha, mem')

--- Free-right-step
expStep (Free (Value (Loc (loc, Memoria))) exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        (Free (Value (Loc (loc, Memoria))) exp', funcs, env', pilha', mem')

--- Free-left-step
expStep (Free exp1 exp2, funcs, env, pilha, mem) = 
    let (exp1', _, env', pilha', mem') = expStep (exp1, funcs, env, pilha, mem) in
        (Free exp1' exp2, funcs, env', pilha', mem')

--- Malloc
expStep (Malloc (Value (Number amount)), funcs, env, pilha, mem) = 
    let (loc, Memoria) = findMem amount mem in
    let mem' = memSet loc amount Mbot mem in 
    (Value (Loc (loc, Memoria)), funcs, env, pilha, mem')

--- Malloc-step
expStep (Malloc exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        (Malloc exp', funcs, env', pilha', mem')


--- ==================================
--- Expressões Aritméticas e Booleanas
--- ==================================

--- As
--- Obtém o número interno do valor e torna ele uma localização do tipo local
expStep (As (Value value) local, funcs, env, pilha, mem) = 
    let number = toNumber value in
        (Value (Loc (number, local)), funcs, env, pilha, mem)

--- As-step
expStep (As exp local, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        (As exp' local, funcs, env', pilha', mem')

--- Not
expStep (Not (Value value), funcs, env, pilha, mem) = 
    let result = Number (if toNumber value /= 0 then 0 else 1)
    in (Value result, funcs, env, pilha, mem)

--- Not-step
expStep (Not exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        (Not exp', funcs, env', pilha', mem')

--- Binop
expStep (Binop op (Value v1) (Value v2), funcs, env, pilha, mem) = 
    let result = binop op v1 v2 
    in (Value result, funcs, env, pilha, mem)

--- Binop-rigth-step
expStep (Binop op (Value v) exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        (Binop op (Value v) exp', funcs, env', pilha', mem')

--- Binop-left-step
expStep (Binop op exp1 exp2, funcs, env, pilha, mem) = 
    let (exp1', _, env', pilha', mem') = expStep (exp1, funcs, env, pilha, mem) in
        (Binop op exp1' exp2, funcs, env', pilha', mem')


--- Deref-memoria
expStep (Deref (Value (Loc (local, Memoria))), funcs, env, pilha, mem) = 
    let value = memGet (local, Memoria) mem in 
        (Value value, funcs, env, pilha, mem)

--- Deref-pilha
expStep (Deref (Value (Loc (local, Pilha))), funcs, env, pilha, mem) = 
    let value = pilhaGet (local, Pilha) pilha in 
        (Value value, funcs, env, pilha, mem)

--- Deref-step
expStep (Deref exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        (Deref exp', funcs, env', pilha', mem')

--- Ref
expStep (Ref varName, funcs, env, pilha, mem) = 
    let loc = envGet varName env in 
        (Value (Loc loc), funcs, env, pilha, mem)

--- Var
expStep (Var varName, funcs, env, pilha, mem) = 
    let loc = envGet varName env in 
    let value = pilhaGet loc pilha in
        (Value value, funcs, env, pilha, mem)


--- ==================================
--- Expressões de Controle
--- ==================================

--- While
--- converte While num if 
expStep (While exp1 exp2, funcs, env, pilha, mem) = 
    (If exp1 (Comp exp2 (While exp1 exp2)) (Value (Number 0)), funcs, env, pilha, mem)

--- If
expStep (If (Value cond) exp1 exp2, funcs, env, pilha, mem) = 
    if toNumber cond /= 0 then
        (exp1, funcs, env, pilha, mem) --true
    else
        (exp2, funcs, env, pilha, mem) --false

--- If-step
expStep (If exp1 exp2 exp3, funcs, env, pilha, mem) = 
    let (exp1', _, env', pilha', mem') = expStep (exp1, funcs, env, pilha, mem) in
        (If exp1' exp2 exp3, funcs, env', pilha', mem')


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
                    (\acc (varName, value) -> 
                        Comp (Comp (Let varName 1) (Assign (Var varName) value)) acc) 
                    body 
                    -- usa-se o reverse pois essa árvore cresce da folha a raiz, ou seja, o primeiro
                    -- elemento da lista vai ser o último na árvore
                    (reverse $ zip 
                        declargs 
                        (Prelude.map -- avalia as expressões para valor (em ordem?)
                            (\exp -> let (e, _, _, _, _) = expStep (exp, funcs, env, pilha, mem) in e) 
                            args
                            ))
                        in
            (Fpop $ Scope expTree, funcs, Stop : env, Pfunc : pilha, mem)

--- Remove as estruturas de controle das pilhas no fim da função
expStep (Fpop (Value v), funcs, env, pilha, mem) = 
    let (_ : env') = env in -- desempilha o topo de env
    let pilha' = pilhaPop Pfunc pilha in  --- remove valores até Pfunc
    (Value v, funcs, env', pilha', mem)

--- Escopo-step
expStep (Fpop exp, funcs, env, pilha, mem) = 
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem) in
        (Fpop exp', funcs, env', pilha', mem')


-- Para a avaliação quando chega em valor
expStep (Value value, funcs, env, pilha, mem) = (Value value, funcs, env, pilha, mem)

--- Anota o operador composição como right associative
--- pois se não, por deafult ele é left e faz a composição errada
--- tornando a .> b.> c em Comp (Comp a b) c
--- ao invés do correto que é Comp a (Comp b c)
--- na real isso não é muito relevante, mas fica mais facil de ler
infixr 6 .>
(.>) :: Exp -> Exp -> Exp
exp1 .> exp2 = Comp exp1 exp2

programComposition = Value (Number 0) .> Value (Number 2) .> Value (Number 4)

programLetAssign = Scope (Let "x" 1 .> Assign (Var "x") (Value (Number 5)))


ex1 = Main $ Let "i" 1 .> 
    Assign (Var "i") (Value (Number 3)) .>
    Let "m" 1 .>
    Assign (Var "m") (Malloc (Var "i")) .>
    While (Binop Less  (Value (Number (-1))) (Assign (Var "i") (Binop Sub (Var "i") (Value (Number 1))))) (
        Assign (Deref (Binop Add (Var "m") (Var "i"))) (Var "i")
    )

ex2 = Main $
    Let "x" 2 .> 
    Assign (Var "x") (Value (Number 1)) .>
    Assign (Deref (Binop Add (Ref "x") (Value (Number 1)))) (Value (Number 2))

ex3 = DeclFunc "square" ["x"] (Binop Mult (Var "x") (Var "x")) $ Main $
    Let "y" 1 .> 
    Assign (Var "y") (Value (Number 5)) .>
    Let "x" 1 .>
    Assign (Var "x") (
        --Value(Number 45)
        CallFunc "square" [Binop Add (Var "y") (Value (Number 1))]
    )

ex4 = Main $
    Assign (Deref (Let "x" 1)) (Value (Number 3)) .> 
    Var "x"

trees = DeclFunc "newNode" ["val"] 
    (Scope (Let "node" 1 .>
    Assign (Var "node") (Malloc (Value (Number 3))) .>
    Assign (Deref (Var "node")) (Var "val") .>
    Assign (Deref (Binop Add (Var "node") (Value (Number 1)))) (As (Value (Number 0)) Memoria) .>
    Assign (Deref (Binop Add (Var "node") (Value (Number 2)))) (As (Value (Number 0)) Memoria) .>
    Var "node"
    )) 
    (DeclFunc "insert" ["tree", "val"] 
        (Scope (
            If (Binop Equal (Var "tree") (Value (Number 0)))
                (CallFunc "newNode" [Var "val"])
                (--else
                    If (Binop Greater (Var "val") (Deref (Var "tree")))
                        (Assign (Deref (Binop Add (Var "tree") (Value (Number 1)))) (CallFunc "insert" [Deref(Binop Add (Var "tree") (Value (Number 1))), Var "val"]))
                        (Assign (Deref (Binop Add (Var "tree") (Value (Number 2)))) (CallFunc "insert" [Deref(Binop Add (Var "tree") (Value (Number 2))), Var "val"]))
                    .>
                    Var "tree"
                )
        ))
    (DeclFunc "get" ["tree", "val"] 
        (Scope (
            If (Binop Equal (Var "tree") (Value (Number 0)))
                (As (Value (Number 0)) Memoria)
                (
                    If (Binop Equal (Var "val") (Deref (Var "tree")))
                        (Var "tree")
                        (
                            If (Binop Greater (Var "val") (Deref (Var "tree")))
                                (CallFunc "get" [Deref (Binop Add (Var "tree") (Value (Number 1))), Var "val"])
                                (CallFunc "get" [Deref (Binop Add (Var "tree") (Value (Number 2))), Var "val"])
                        )
                )
        ))
    (Main $ 
        Let "tree" 1 .>
        Assign (Var "tree") (As (Value (Number 0)) Memoria) 
        .>
        Assign (Var "tree") (CallFunc "insert" [Var "tree", Value (Number 5)]) .>
        Assign (Var "tree") (CallFunc "insert" [Var "tree", Value (Number 1)]) .>
        Assign (Var "tree") (CallFunc "insert" [Var "tree", Value (Number 7)]) .>
        Assign (Var "tree") (CallFunc "insert" [Var "tree", Value (Number 4)]) .>

        Let "branch" 1 .>
        Assign (Var "branch") (CallFunc "get" [Var "tree", Value (Number 7)]) 
    )))


testfunc = DeclFunc "0" ["var1", "var2"] 
    (Scope (Binop Add (Var "var1") (Var "var2"))) $ Main $
        Let "x" 1 .>
        Assign (Var "x")  (CallFunc "0" [Value (Loc (2, Memoria)), Value (Number 3)]) .>
        Assign (Var "x")  (CallFunc "0" [Value (Loc (7, Memoria)), Value (Number 9)]) 

--- Para salvar uma computação, pode-se ao invés de começar com um (Scope e, F, [], [], [])
--- pode-se omitir o scope e adicionar espaço diretamente nos mapas, 
--- q assim não será deletado no fim do programa

execLoop :: IO (Exp, Funcs, Env, Pilha, Mem) -> IO (Exp, Funcs, Env, Pilha, Mem)

execLoop state = do
    (exp, funcs, env, pilha, mem) <- state --obtêm o passo de execução
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem)
    putStrLn "\nProgram"
    print exp'
    putStrLn "Estados"
    print env'
    print pilha'
    print mem'
    case exp' of 
        Value _ -> return (exp', funcs, env', pilha', mem')
        _ -> do 
            putStrLn "Aperte qualquer tecla para o próximo passo"
            _  <- getChar
            execLoop (return (exp', funcs, env', pilha', mem'))

execFull state = do
    (exp, funcs, env, pilha, mem) <- state --obtêm o passo de execução
    let (exp', _, env', pilha', mem') = expStep (exp, funcs, env, pilha, mem)
    case exp' of 
        Value _ -> do
            print env'
            print pilha'
            print mem'
            return (exp', funcs, env', pilha', mem')
        _ -> execFull (return (exp', funcs, env', pilha', mem'))


main = do 
    {-
    print programComposition
    print (expStep  (programComposition, empty, [], [], []))
    print programLetAssign
    print "====="
    print (expStep (programLetAssign, empty, [], [], []))
    --print ex3
    let (Main exp, funcs) = functionStep (trees, empty)
    print exp
    execFull $ return (exp, funcs, [Frame empty], [Pstack], [])
    -}

    let (Main exp, funcs) = functionStep (trees, empty)
    print exp
    execLoop $ return (exp, funcs, [Frame empty], [Pstack], [])
    --print exp
    --print (Main exp, funcs, env, pilha, mem) 
    --let (res, _, env, pilha, mem) = expStep (exp, funcs, [Frame empty], [Pstack], [])