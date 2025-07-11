module Mem(new, get, insert, malloc, free, Mem, Values(..)) where

import Data.Bifunctor(first)

import Pcl


data Values = Loc Pcl.Loc | Num Pcl.Number | Bot | Null deriving (Show, Eq)

-- segundo valor é o contador monotônico, 
-- eu ia usar uma global, mas haskell me assutou e disse não faz isso
type Mem = ([(Values, Int)], Int)


new :: Mem
new = ([], 1)

getMv :: Int -> Mem -> (Values, Int)
getMv _ ([], _) = (Null, 0)
getMv 0 (h : _, _) = h
getMv loc (_ : t, mono) = getMv (loc - 1) (t, mono)

--- Obtém o valor sintático da memória
get :: Loc -> Mem -> Either ErrorKinds Value

get loc mem 
    | local loc == Pilha = error "chamada da função Mem.get com um endereço de pilha"
    | idx loc == 0 = Left NullPtrDereference
    | not $ isInBounds loc = Left OutOfBoundsRead
    | key loc /= lock = Left UseAfterFree
    | otherwise = case value of
        Mem.Loc loc' -> Right $ Pcl.Loc loc'
        Mem.Num number -> Right $ Pcl.Number number 
        Mem.Bot -> Left UninitializedAcess
        -- com um ponteiro vivo e in-bounds, não há como o valor da célula ser Null
        Mem.Null -> error "Mem.get: Valor da célula de memória nesta posição não tem como ter valor -"
    where 
        (value, lock) = getMv (idx loc + offset loc) mem

--- Coloca o MemValue no Loc da Memória
insertMv :: Int -> Values -> Mem -> Maybe Mem

insertMv 0 value (h : t, mono) = Just ((value, snd h) : t, mono)
insertMv loc value (h : t, mono) = do 
    (rest, mono') <- insertMv (loc - 1) value (t, mono)
    Just (h : rest, mono')
insertMv _ _ ([],_) = Nothing

--- Coloca um Value na memória
--- quase que exatamente igual a pilhaSet
insert :: Loc -> Value -> Mem -> Either ErrorKinds Mem
insert loc value mem
    | local loc == Pilha = error "chamada da função Mem.insert com um endereço de pilha"
    | idx loc == 0 = Left NullPtrDereference
    | not (isInBounds loc) = Left OutOfBoundsWrite
    | key loc /= lock = Left UseAfterFree -- se lock é valido, então a memória não pode ser -
    | otherwise = maybe
        (error "Mem.set: a memória nessa região deveria estar alocada")
        Right
        (insertMv (idx loc + offset loc) (case value of 
            Pcl.Number number -> Num number
            Pcl.Loc loc' -> Mem.Loc loc'
        ) mem)
    where 
        (_, lock) = getMv (idx loc + offset loc) mem


malloc :: Int -> Mem -> (Pcl.Loc, Mem)
malloc amount (memValues, mono) = 
    let locIdx = find amount (memValues, mono) in
    let newMem = set locIdx amount (Bot, mono) memValues in
    (newLoc Memoria locIdx mono amount, (newMem, mono + 1))

-- check if loc is in Mem
-- check if the pointer doesnt overflow
-- check if the key at local and for every amount matches the key of loc
-- check if after the operation if the key still exists in the mem
free :: Pcl.Loc -> Int -> Mem -> Either ErrorKinds Mem
free loc amount mem
    | local loc == Pilha = Left FreeOfMemoryNotOnHeap
    | idx loc == 0 = Left FreeOfMemoryNotOnHeap
    | offset loc > 0 || size loc /= amount = Left PartialFree
    | key loc /= lock = Left DoubleFree
    | otherwise = Right $ first (set (idx loc) amount (Null, 0)) mem
    where 
        (_, lock) = getMv (idx loc) mem


--- Insere n valores de memória apartir de l em mem
--- loc -> size -> value -> memoria
set :: Pcl.Number -> Int -> (Values, Int) -> [(Values, Int)] -> [(Values, Int)]

-- finished
set 0 0 _ mem = mem    
-- se chegou no local, começa trocar a cabeça por mv
set 0 n mv (_ : t) = mv : set 0 (n - 1) mv t 
-- se chegou no local e não tem nada, insere mv no fim
set 0 n mv [] = mv : set 0 (n - 1) mv []     
-- se não chegou no local, segue a lista
set idx n mv (h : t) = h : set (idx - 1) n mv t  
-- se chegou ao fim da lista, mas não no local, insere Null até chegar no local
set idx n mv [] = (Null, 0) : set (idx - 1) n mv []  



--- encontra Int espaço consecutivos vagos na memória Mem, retornando Loc.
--- a função pode retornar um Loc = size(Mem), nesse caso, 
--- a memória não contém espaço vago no momento e deve ser exprandida
find :: Int -> Mem -> Number
find size mem = findMem' 0 1 
    where
    findMem' count idx = case getMv (idx + count) mem of
        -- se mem(idx + count) não estiver sido alocado, aumenta a quantidade achada em 1 até chegar em size
        (Null, _) -> if size == count then idx
            else findMem' (count + 1) idx
        -- se, em algum momento, a memória não for Null, ou seja, está alocada, reseta o count e segue de idx + count + 1
        _ -> findMem' 0 (idx + count + 1)