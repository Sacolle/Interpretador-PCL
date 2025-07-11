module Pilha(Values(..), Pilha, new, get, set, push, pushCtrl, pop) where

import Pcl

--- Pilha de Valores

data Values = Loc Pcl.Loc | Num Pcl.Number | Bot | Null | Stack | Func deriving (Show, Eq)
type Pilha = ([(Values, Int)], Int)

new :: Pilha
new = ([], 1)


--- Obtém o valor da pilha da pilha de valores
--- indexa com (length pilha) o topo e 0 a base.
getMv :: Pcl.Number -> Pilha -> (Values, Int)

getMv loc (pilha, _) = getMv' (length pilha - 1) pilha
    where
    getMv'  _ [] = (Null, 0)
    getMv' size (h : t) = if loc == size then h
        else getMv' (size - 1) t


--- Obtém o valor sintático da pilha de valores
get :: Pcl.Loc -> Pilha -> Either ErrorKinds Value

get loc pilha 
    | local loc == Memoria = error "chamada da função pilhaGet com um endereço de memória"
    | not $ isInBounds loc = Left OutOfBoundsRead 
    | lock /= key loc = Left UseAfterFree
    | otherwise = case value of 
        Pilha.Loc ploc -> Right $ Pcl.Loc ploc
        Pilha.Num number -> Right $ Pcl.Number number 
        Pilha.Bot -> Left UninitializedAcess
        -- Esses 3 casos nunca acontecem, pois se chegou neste otherwise
        -- a) o ponteiro está dentro do espaço alocado (não erro espacial)
        -- b) a região de memória está viva (não temporal)
        -- Como não se coloca valor de controle em espaço alocado
        -- e como na alocação atribui-se \bot, dado os elementos acima
        -- não tem como uma de-referencia de um ponteiro in-bounds vivo resultar nos demais valores 
        rest -> error ("Pilha.get: o valor na região de memória não poderia ser " ++ show rest)
        -- Pilha.Null -> Left UninitializedStackAcess 
        -- Pilha.Stack -> Left ControlValueStackAcess
        -- Pilha.Func -> Left ControlValueStackAcess
    where
        (value, lock) = getMv (idx loc + offset loc) pilha

--- Coloca o Value no Loc da Pilha
setMv :: Int -> Values -> Pilha -> Maybe Pilha

setMv loc value (pilha, mono) = do 
    newPilha <- setMv' (length pilha - 1) pilha
    Just (newPilha, mono)
    where
        setMv' _ [] = Nothing
        setMv' size  (h : t)
            | loc == size = Just $ (value, snd h) : t
            | otherwise = do 
                rest <- setMv' (size - 1) t
                Just $ h : rest

--- Coloca o Value no Loc da Pilha
set :: Loc -> Value -> Pilha -> Either ErrorKinds Pilha
--- Parece o demônio da babilônia, mas faz a sequência:
--- Checa se o local de inserção é valido (se é bot, num ou local)
--- se é, coloca o valor como valor de pilha
set loc value pilha 
    | local loc == Memoria = error "chamada da função pilhaSet com um endereço de memória"
    | not $ isInBounds loc = Left OutOfBoundsWrite
    | key loc /= lock = Left UseAfterFree
    | otherwise = maybe 
        (error "Mem.set: A pilha deveria existir no momento da inserção de valor") 
        Right 
        (setMv (idx loc + offset loc) (case value of 
            Pcl.Number number -> Num number
            Pcl.Loc loc' -> Pilha.Loc loc'
        ) pilha)
    where 
        (_, lock) = getMv (idx loc + offset loc) pilha

-- Usado para adicionar valores de controle na pilha
pushCtrl :: Values -> Pilha -> Pilha
pushCtrl value (pilha, mono) = ((value, 0) : pilha, mono)
    

push :: Int -> Values -> Pilha -> (Loc, Pilha)
push amount value (pilha, mono) = 
    let topo = length pilha in
        (newLoc Pilha topo mono amount, 
        (replicate amount (value, mono) ++ pilha, mono + 1))

--- Remove o topo da pilha até o valor value, esse sempre será Pstack ou Pfunc
-- considere value como Pstack
--Topo
-- V
-- 4 : 7 : Pstack : 8 : 0 : nil - não é Pstack, remove e continua
-- 7 : Pstack : 8 : 0 : nil - não é Pstack, remove e continua
-- Pstack : 8 : 0 : nil para e retorna 8 : 0 : nil

pop :: Values -> Pilha -> Pilha
pop value ((pValue, _) : t, mono)
    | pValue /= value = pop value (t, mono)
    | otherwise = (t, mono)
pop _ ([], mono) = ([], mono)

