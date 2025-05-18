module Mem(new, get, insert, malloc, free, Mem, Values(..)) where

import Data.Bifunctor(first)

import Pcl


data Values = Loc Pcl.Loc | Num Pcl.Number | Bot | Null deriving (Show, Eq)

-- segundo valor é o contador monotônico, 
-- eu ia usar uma global, mas haskell me assutou e disse não faz isso
type Mem = ([(Values, Int)], Int)


new :: Mem
new = ([], 1)

getMv :: Int -> Mem -> Maybe (Values, Int)
getMv _ ([], _) = Nothing
getMv 0 (h : _, _) = Just h
getMv loc (_ : t, mono) = getMv (loc - 1) (t, mono)

--- Obtém o valor sintático da memória
get :: Loc -> Mem -> Either ErrorKinds Value

get loc mem 
    | local loc == Pilha = error "chamada da função Mem.get com um endereço de pilha"
    | not $ isInBounds loc = Left OutOfBoundsRead
    | otherwise = maybe (Left UninitializedMemoryAcess) parseValue (getMv (idx loc + offset loc) mem) 
    where 
        parseValue (value, lock)
            | key loc /= lock = Left UseAfterFree
            | otherwise = case value of
                Mem.Loc loc' -> Right $ Pcl.Loc loc'
                Mem.Num number -> Right $ Pcl.Number number 
                Mem.Null -> Left UninitializedMemoryAcess
                Mem.Bot -> Left InitializedButEmptyMemoryAcess

--- Coloca o MemValue no Loc da Memória
insertMv :: Int -> Values -> Mem -> Either ErrorKinds Mem

insertMv 0 value (h : t, mono) = Right ((value, snd h) : t, mono)
insertMv loc value (h : t, mono) = do 
    (rest, mono') <- insertMv (loc - 1) value (t, mono)
    Right (h : rest, mono')
insertMv _ _ ([],_) = Left UninitializedMemoryWrite

--- Coloca um Value na memória
--- quase que exatamente igual a pilhaSet
insert :: Loc -> Value -> Mem -> Either ErrorKinds Mem
insert loc value mem
    | local loc == Pilha = error "chamada da função Mem.insert com um endereço de pilha"
    | not (isInBounds loc) = Left OutOfBoundsWrite
    | otherwise = maybe (Left UninitializedMemoryWrite) parseValue (getMv (idx loc + offset loc) mem)
    where 
        parseValue (_, lock) 
            | key loc /= lock = Left UseAfterFree -- se lock é valido, então a memória não pode ser -
            | otherwise = 
                insertMv (idx loc + offset loc) (case value of 
                    Pcl.Number number -> Num number
                    Pcl.Loc loc' -> Mem.Loc loc'
                ) mem


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
    | not (isInBounds loc) = Left OutOfBoundsFree
    | offset loc > 0 || size loc /= amount = Left PartialFree
    | otherwise = maybe (Left UninitializedFree) freeMem (getMv (idx loc) mem)
    where 
        freeMem (_, lock)
            | key loc /= lock = Left DoubleFree
            | otherwise = Right $ first (set (idx loc) amount (Null, 0)) mem


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
        Nothing -> idx -- chegou no fim da memória
        --- se, com uma célula vaga, achou o espaço, retorna l, se não, aumenta count e segue pro próximo
        Just (Null, _) -> if size == count then idx
        else findMem' (count + 1) idx
        Just _ -> findMem' 0 (idx + 1)