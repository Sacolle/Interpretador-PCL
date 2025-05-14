{-# LANGUAGE LambdaCase #-}

module Mem(get, set, insert, find, Mem, Values(..)) where

import Pcl
--- Memória

data Values = Loc Pcl.Loc | Num Pcl.Number | Bot | Null deriving (Show, Eq)
type Mem = [Values]

--- Obtém o valor de memória da memória
getMv :: Pcl.Number -> Mem -> Maybe Values

getMv _ [] = Nothing
getMv 0 (h : _) = Just h
getMv loc (_ : t) = getMv (loc - 1) t

--- Obtém o valor sintático da memória
get :: Loc -> Mem -> Value

get (Memoria loc) mem = maybe 
    (error "Local inexistente na memória")
    (\case 
        Mem.Loc loc' -> Pcl.Loc loc'
        Num number -> Pcl.Number number 
        val -> error ("Conversão de memória inválida.\nTentativa de converter: " ++ show val)
    )
    (getMv loc mem)

get (Pilha _) _= error "chamada da função memGet com um endereço de pilha"

--- Coloca o MemValue no Loc da Memória
insertMv :: Int -> Values -> Mem -> Mem

insertMv 0 value (_ : t) = value : t
insertMv loc value (h : t) = h : insertMv (loc - 1) value t
insertMv _ _ [] = error "Index Out of bounds na memória"

--- Coloca um Value na memória
--- quase que exatamente igual a pilhaSet
insert :: Loc -> Value -> Mem -> Mem
insert (Memoria loc) value mem
    | isValidInsertion = 
        insertMv loc (case value of 
            Pcl.Number number -> Num number
            Pcl.Loc loc' -> Mem.Loc loc'
        ) mem
    | otherwise = error "Inserção em local de memória inválido"
    where
        isValidInsertion = maybe False (\case 
            Null -> False
            _ -> True) 
            (getMv loc mem) 

insert (Pilha _) _ _= error "chamada da função Mem.insert com um endereço de pilha"


--- Insere n valores de memória apartir de l em mem
--- loc -> size -> value -> memoria
set :: Pcl.Number -> Int -> Values -> Mem -> Mem
-- finished
set 0 0 _ mem = mem    
-- se chegou no local, começa trocar a cabeça por mv
set 0 n mv (_ : t) = mv : set 0 (n - 1) mv t 
-- se chegou no local e não tem nada, insere mv no fim
set 0 n mv [] = mv : set 0 (n - 1) mv []     
-- se não chegou no local, segue a lista
set loc n mv (h : t) = h : set (loc - 1) n mv t  
-- se chegou ao fim da lista, mas não no local, insere Null até chegar no local
set loc n mv [] = Null : set (loc - 1) n mv []  

--- encontra Int espaço consecutivos vagos na memória Mem, retornando Loc.
--- a função pode retornar um Loc = size(Mem), nesse caso, 
--- a memória não contém espaço vago no momento e deve ser exprandida
find :: Int -> Mem -> Number
find size mem = findMem' 0 1 
    where
    findMem' count loc = case getMv (loc + count) mem of
        Nothing -> loc -- chegou no fim da memória
        --- se, com uma célula vaga, achou o espaço, retorna l, se não, aumenta count e segue pro próximo
        Just Null -> if size == count then loc 
        else findMem' (count + 1) loc 
        Just _ -> findMem' 0 (loc + 1)