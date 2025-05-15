{-# LANGUAGE LambdaCase #-}

module Interpreter where

import Control.Monad (void)
import Env (Env, setGlobal, push, pop, newFrame, set, get, Values(Stop), new)
import Funcs(Funcs, get, set, new)
import Pilha(Values(Stack, Bot, Func), Pilha, pop, set, get)
import Mem(Values(Null, Bot), Mem, set, get, insert, find)
import Pcl

--- Função auxiliar para a avaliação das exprressões aritméticas e booleanas

binop :: Binop -> Value -> Value -> Value
--- no binop, ele só converte os número e ajeita o tipo de saída
--- nota-se que não existe lp + lm ou vice versa
binop op (Number n1) (Number n2) = Number (binop' op n1 n2)
binop op (Number num) (Loc (Pilha loc)) = Loc $ Pilha $ binop' op num loc
binop op (Loc (Pilha loc)) (Number num) = Loc $ Pilha $ binop' op num loc
binop op (Loc (Pilha loc1)) (Loc (Pilha loc2)) = Loc $ Pilha $ binop' op loc1 loc2
binop op (Number num) (Loc (Memoria loc)) = Loc $ Memoria $ binop' op num loc
binop op (Loc (Memoria loc)) (Number num) = Loc $ Memoria $ binop' op num loc
binop op (Loc (Memoria loc1)) (Loc (Memoria loc2)) = Loc $ Memoria $ binop' op loc1 loc2
binop _ _ _ = error "Operação não definida entre os operandos"

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
    Loc l -> case l of
        Pilha loc -> loc
        Memoria loc -> loc


--- Semântica operacional

-- Em haskell, declarações nesse padrão devem ser declaradas
--  do mais específicos ao menos específico.



exprStep :: (Exp, Funcs, Env, Pilha, Mem) -> (Exp, Funcs, Env, Pilha, Mem)

--- ====================
--- Regras de composição
--- ====================

--- Compose-combine
exprStep (Comp (Value _) expr, funcs, env, pilha, mem) = 
    (expr, funcs, env, pilha, mem)

--- Compose-step
exprStep (Comp expr1 expr2, funcs, env, pilha, mem) = 
    let (expr1', _, env', pilha', mem') = exprStep (expr1, funcs, env, pilha, mem) in
        (Comp expr1' expr2, funcs, env', pilha', mem')


--- Escopo-init
--- Empilha os elementos nas pilha e muda o código para Scope' 
--- para isso ser feito só no primeiro passo
exprStep (Scope expr, funcs, env, pilha, mem) = 
    (Pop expr, funcs, Env.push newFrame env, Pilha.Stack : pilha, mem)

--- Escopo-pop
--- remove-se o frame do topo do ambiente de nomes e 
--- remove elementos da pilha até o código de controle Pop
exprStep (Pop (Value v), funcs, env, pilha, mem) = 
    let (_, env') = Env.pop env in -- desempilha o topo de env
    let pilha' = Pilha.pop Pilha.Stack pilha in  --- remove valores até Stack
    (Value v, funcs, env', pilha', mem)

--- Escopo-step
exprStep (Pop expr, funcs, env, pilha, mem) = 
    let (expr', _, env', pilha', mem') = exprStep (expr, funcs, env, pilha, mem) in
        (Pop expr', funcs, env', pilha', mem')


--- ====================
--- Regras de atribuição
--- ====================

--- Let
exprStep (Let nome size, funcs, env, pilha, mem) = 
    let topoPilha = length pilha in
    let env' = Env.set nome (Pilha topoPilha) env in -- coloca nome -> local no env de nomes
    let pilha' = replicate size Pilha.Bot ++ pilha  in -- empilha size Pbot na pilha (tamanho da variável)
    (Value (Loc (Pilha topoPilha)), funcs, env', pilha', mem)

--- Atribui-deref-pilha
--- *lp := value
exprStep (Assign (Deref (Value (Loc (Pilha local)))) (Value value), funcs, env, pilha, mem) = 
    case Pilha.set (Pilha local) value pilha of 
        Left err -> (Panic err, funcs, env, pilha, mem) -- se houve um erro na inserção, gera um pânico do erro
        Right pilha' -> (Value value, funcs, env, pilha', mem) -- se não atualiza a pilha

--- Atribui-deref-mem
--- *lm := value
exprStep (Assign (Deref (Value (Loc (Memoria local)))) (Value value), funcs, env, pilha, mem) = 
    case Mem.insert (Memoria local) value mem of
        Left err -> (Panic err, funcs, env, pilha, mem)
        Right mem' -> (Value value, funcs, env, pilha, mem')

--- Atribui-var
--- x := value
exprStep (Assign (Var varName) (Value value), funcs, env, pilha, mem) = 
    let local = Env.get varName env in
    case Pilha.set local value pilha of
        Left err -> (Panic err, funcs, env, pilha, mem)
        Right pilha' -> (Value value, funcs, env, pilha', mem)

--- Atribui-deref-right-step
exprStep (Assign (Deref (Value (Loc loc))) expr, funcs, env, pilha, mem) = 
    let (expr', _, env', pilha', mem') = exprStep (expr, funcs, env, pilha, mem) in
        (Assign (Deref (Value (Loc loc))) expr', funcs, env', pilha', mem')

--- Atribui-var-right-step
exprStep (Assign (Var varName) expr, funcs, env, pilha, mem) = 
    let (expr', _, env', pilha', mem') = exprStep (expr, funcs, env, pilha, mem) in
        (Assign (Var varName) expr', funcs, env', pilha', mem')

--- Atribui-deref-left-step
exprStep (Assign (Deref expr1) expr2, funcs, env, pilha, mem) = 
    let (expr1', _, env', pilha', mem') = exprStep (expr1, funcs, env, pilha, mem) in
        (Assign (Deref expr1') expr2, funcs, env', pilha', mem')


--- ======================
--- Manipulação de memória
--- ======================

--- Free
exprStep (Free (Value (Loc (Memoria loc))) (Value (Number num)), funcs, env, pilha, mem) = 
    let mem' = Mem.set loc num Mem.Null mem in 
    (Value (Number num), funcs, env, pilha, mem')

--- Free-right-step
exprStep (Free (Value (Loc (Memoria loc))) expr, funcs, env, pilha, mem) = 
    let (expr', _, env', pilha', mem') = exprStep (expr, funcs, env, pilha, mem) in
        (Free (Value (Loc (Memoria loc))) expr', funcs, env', pilha', mem')

--- Free-left-step
exprStep (Free expr1 expr2, funcs, env, pilha, mem) = 
    let (expr1', _, env', pilha', mem') = exprStep (expr1, funcs, env, pilha, mem) in
        (Free expr1' expr2, funcs, env', pilha', mem')

--- Malloc
exprStep (Malloc (Value (Number amount)), funcs, env, pilha, mem) = 
    let loc = Mem.find amount mem in
    let mem' = Mem.set loc amount Mem.Bot mem in 
    (Value (Loc (Memoria loc)), funcs, env, pilha, mem')

--- Malloc-step
exprStep (Malloc expr, funcs, env, pilha, mem) = 
    let (expr', _, env', pilha', mem') = exprStep (expr, funcs, env, pilha, mem) in
        (Malloc expr', funcs, env', pilha', mem')


--- ==================================
--- Expressões Aritméticas e Booleanas
--- ==================================
--- Not
exprStep (Not (Value value), funcs, env, pilha, mem) = 
    let result = Number (if toNumber value /= 0 then 0 else 1)
    in (Value result, funcs, env, pilha, mem)

--- Not-step
exprStep (Not expr, funcs, env, pilha, mem) = 
    let (expr', _, env', pilha', mem') = exprStep (expr, funcs, env, pilha, mem) in
        (Not expr', funcs, env', pilha', mem')

--- Binop
exprStep (Binop op (Value v1) (Value v2), funcs, env, pilha, mem) = 
    let result = binop op v1 v2 
    in (Value result, funcs, env, pilha, mem)

--- Binop-rigth-step
exprStep (Binop op (Value v) expr, funcs, env, pilha, mem) = 
    let (expr', _, env', pilha', mem') = exprStep (expr, funcs, env, pilha, mem) in
        (Binop op (Value v) expr', funcs, env', pilha', mem')

--- Binop-left-step
exprStep (Binop op expr1 expr2, funcs, env, pilha, mem) = 
    let (expr1', _, env', pilha', mem') = exprStep (expr1, funcs, env, pilha, mem) in
        (Binop op expr1' expr2, funcs, env', pilha', mem')


--- Deref-memoria
exprStep (Deref (Value (Loc (Memoria loc))), funcs, env, pilha, mem) = 
    case Mem.get (Memoria loc) mem of
        Left err -> (Panic err, funcs, env, pilha, mem)
        Right value -> (Value value, funcs, env, pilha, mem)

--- Deref-pilha
exprStep (Deref (Value (Loc (Pilha loc))), funcs, env, pilha, mem) = 
    case Pilha.get (Pilha loc) pilha of 
        Left err -> (Panic err, funcs, env, pilha, mem)
        Right value -> (Value value, funcs, env, pilha, mem)

--- Deref-step
exprStep (Deref expr, funcs, env, pilha, mem) = 
    let (expr', _, env', pilha', mem') = exprStep (expr, funcs, env, pilha, mem) in
        (Deref expr', funcs, env', pilha', mem')

--- Ref
exprStep (Ref varName, funcs, env, pilha, mem) = 
    let loc = Env.get varName env in 
        (Value (Loc loc), funcs, env, pilha, mem)

--- Var
exprStep (Var varName, funcs, env, pilha, mem) = 
    let loc = Env.get varName env in 
    case Pilha.get loc pilha of
        Left err -> (Panic err, funcs, env, pilha, mem)
        Right value -> (Value value, funcs, env, pilha, mem)


--- ==================================
--- Expressões de Controle
--- ==================================

--- While
--- converte While num if 
exprStep (While expr1 expr2, funcs, env, pilha, mem) = 
    (If expr1 (Comp expr2 (While expr1 expr2)) (Value (Number 0)), funcs, env, pilha, mem)

--- If
exprStep (If (Value cond) expr1 expr2, funcs, env, pilha, mem) = 
    if toNumber cond /= 0 then
        (expr1, funcs, env, pilha, mem) --true
    else
        (expr2, funcs, env, pilha, mem) --false

--- If-step
exprStep (If expr1 expr2 expr3, funcs, env, pilha, mem) = 
    let (expr1', _, env', pilha', mem') = exprStep (expr1, funcs, env, pilha, mem) in
        (If expr1' expr2 expr3, funcs, env', pilha', mem')


--- ==================================
--- Chamada de função
--- ==================================

exprStep (CallFunc funcName args, funcs, env, pilha, mem)
    | length declargs /= length args = 
        error "Chamada de função com o número errado de argumentos"
    | evaluatedAllList = let exprTree = Prelude.foldl foldTree body (reverse $ zip declargs args) in
        (Fpop $ Scope exprTree, funcs, Env.push Env.Stop env, Pilha.Func : pilha, mem)
    | otherwise = let (evaluatedArgs, funcs', env', pilha', mem') = evaluateNext (args, funcs, env, pilha, mem) in
        (CallFunc funcName evaluatedArgs, funcs', env', pilha', mem')
    where 
        (declargs, body) = Funcs.get funcName funcs
        evaluatedAllList = Prelude.all (\case Value _ -> True; _ -> False) args
        foldTree acc (varName, value) = Comp (Comp (Let varName 1) (Assign (Var varName) value)) acc

        evaluateNext ([], _, _, _, _) = error "Padrão inacessível em avaliação de argumentos"
        evaluateNext (Value v : t, _, _ , _, _) = 
            let (args', funcs', env', pilha', mem') = evaluateNext (t, funcs, env, pilha, mem) in
            (Value v : args', funcs', env', pilha', mem')
        evaluateNext (h : t, _, _ , _, _) = 
            let (expr, funcs', env', pilha', mem') = exprStep (h, funcs, env, pilha, mem) in
            (expr : t, funcs', env', pilha', mem')

--- Remove as estruturas de controle das pilhas no fim da função
exprStep (Fpop (Value v), funcs, env, pilha, mem) = 
    let (_ , env') = Env.pop env in -- desempilha o topo de env
    let pilha' = Pilha.pop Pilha.Func pilha in  --- remove valores até Pfunc
    (Value v, funcs, env', pilha', mem)

--- Escopo-step
exprStep (Fpop expr, funcs, env, pilha, mem) = 
    let (expr', _, env', pilha', mem') = exprStep (expr, funcs, env, pilha, mem) in
        (Fpop expr', funcs, env', pilha', mem')


-- Para a avaliação quando chega em valor
exprStep (Value value, funcs, env, pilha, mem) = (Value value, funcs, env, pilha, mem)

-- exprStep não é exaustivo, então se cair nessa função é meio que um fallthrogh
exprStep (expr, _, _, _,_) = error $ "Não há derivação para a expressão: " ++ show expr

globalStep :: (Global, Env, Pilha) -> (Global, Env, Pilha)
globalStep (DeclGlobal nome size global, env, pilha) =
    let topoPilha = length pilha in
    let env' = Env.setGlobal nome (Pilha topoPilha) env in
    let pilha' = replicate size Pilha.Bot ++ pilha  in -- empilha size Pbot na pilha (tamanho da variável)
    (global, env', pilha')
globalStep (Pcl.Func f, env, pilha) = (Pcl.Func f, env, pilha)


globalFullStep :: (Global, Env, Pilha) -> (Function, Env, Pilha)
globalFullStep global = case globalStep global of
    (Pcl.Func func, env, pilha) -> (func, env, pilha)
    g -> globalFullStep g

-- Passa por todas as funções e salva ela em Funcss. Para quando chega na Main
functionStep :: (Function, Funcs) -> (Function, Funcs)
functionStep (DeclFunc funcname args body function, funcs) = 
    (function, Funcs.set funcname (args, body) funcs)
functionStep (Main expr, funcs) = (Main expr, funcs)

functionFullStep :: (Function, Funcs) -> (Exp, Funcs)
functionFullStep f = case functionStep f of
    (Main expr, funcs) -> (expr, funcs)
    func -> functionFullStep func




fullRun :: Ast -> IO ()
fullRun globals = do 
    let (func, env, pilha) = globalFullStep (globals, Env.new, [])
    let (expr, funcs) = functionFullStep (func, Funcs.new)
    void $ execFull $ return (Scope expr, funcs, env, pilha, [])
    where
    execFull state = do
        (expr, funcs, env, pilha, mem) <- state --obtêm o passo de execução
        let (expr', _, env', pilha', mem') = exprStep (expr, funcs, env, pilha, mem)
        case expr' of 
            Value _ -> do
                print env'
                print pilha'
                print mem'
                return (expr', funcs, env', pilha', mem')
            _ -> execFull (return (expr', funcs, env', pilha', mem'))


stepRun :: Ast -> IO ()
stepRun globals = do 
    let (func, env, pilha) = globalFullStep (globals, Env.new, [])
    let (expr, funcs) = functionFullStep (func, Funcs.new)
    void $ execLoop $ return (Scope expr, funcs, env, pilha, [])
    where
    execLoop state = do
        (expr, funcs, env, pilha, mem) <- state --obtêm o passo de execução
        let (expr', _, env', pilha', mem') = exprStep (expr, funcs, env, pilha, mem)
        putStrLn "\nProgram"
        print expr'
        putStrLn "Estados"
        print env'
        print pilha'
        print mem'
        print funcs
        case expr' of 
            Value _ -> return (expr', funcs, env', pilha', mem')
            _ -> do 
                putStrLn "Aperte qualquer tecla para o próximo passo"
                _  <- getChar
                execLoop (return (expr', funcs, env', pilha', mem'))