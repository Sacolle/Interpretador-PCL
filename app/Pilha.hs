{-# LANGUAGE LambdaCase #-}

module Pilha(Values(..), Pilha, get, set, pop) where

import Pcl(Number, Loc(Pilha, Memoria), Value(Number, Loc))

--- Pilha de Valores

data Values = Loc Pcl.Loc | Num Pcl.Number | Bot | Null | Stack | Func deriving (Show, Eq)
type Pilha = [Values]


--- Obtém o valor da pilha da pilha de valores
--- indexa com (length pilha) o topo e 0 a base.
getMv :: Pcl.Number -> Pilha -> Maybe Values

getMv loc pilha = getMv' (length pilha - 1) pilha
    where
    getMv'  _ [] = Nothing
    getMv' size (h : t) = if loc == size then Just h
        else getMv' (size - 1) t


--- Obtém o valor sintático da pilha de valores
get :: Pcl.Loc -> Pilha -> Value

get (Pilha loc) pilha = maybe 
    (error "Local inexistente na memória")
    (\case --se pilhaGetMv retorna Just, aplica essa função
        Pilha.Loc ploc -> Pcl.Loc ploc
        Num number -> Pcl.Number number 
        val -> error ("Conversão de memória inválida.\nTentativa de converter: " ++ show val)
    )
    (getMv loc pilha)

get (Memoria _) _ = error "chamada da função pilhaGet com um endereço de memória"

--- Coloca o PiValue no Loc da Pilha
setMv :: Int -> Values -> Pilha -> Pilha

setMv loc value pilha = setMv' (length pilha - 1) pilha
    where
    setMv' _ [] = error "Index Out of bounds na pilha"
    setMv' size  (h : t)
        | loc == size = value : t
        | otherwise = h : setMv' (size - 1) t

--- Coloca o Value no Loc da Pilha
set :: Loc -> Value -> Pilha -> Pilha
--- Parece o demônio da babilônia, mas faz a sequência:
--- Checa se o local de inserção é valido (se é bot, num ou local)
--- se é, coloca o valor como valor de pilha
set (Pilha loc) value pilha 
    | isValidInsertion = 
        setMv loc (case value of 
            Pcl.Number number -> Num number
            Pcl.Loc loc' -> Pilha.Loc loc'
        ) pilha
    | otherwise = error "Inserção em local de memória inválido"
    where 
        isValidInsertion = maybe False (\case 
            Num _ -> True
            Pilha.Loc _ -> True
            Bot -> True 
            _ -> False) 
            (getMv loc pilha) 

set (Memoria _) _ _ = error "chamada da função pilhaSet com um endereço de memória"
    

--- Remove o topo da pilha até o valor value, esse sempre será Pstack ou Pfunc
-- considere value como Pstack
--Topo
-- V
-- 4 : 7 : Pstack : 8 : 0 : nil - não é Pstack, remove e continua
-- 7 : Pstack : 8 : 0 : nil - não é Pstack, remove e continua
-- Pstack : 8 : 0 : nil para e retorna 8 : 0 : nil

pop :: Values -> Pilha -> Pilha
pop value (h : t) = if h /= value then pop value t else t
pop _ [] = []

