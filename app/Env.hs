module Env(Env, Values(Stop), get, set, setGlobal, newFrame, push, pop, new) where

import Data.Map (Map, insert, lookup, toList, empty )
import Pcl (VarName, Loc)

--- Ambiente de nomes
data Values = Frame (Map VarName Loc) | Stop
type Globals = Map VarName Loc
type Env = ([Values], Globals)

instance Show Values where
    show value = case value of 
        Stop -> "Stop"
        Frame mapa -> "{" ++ Prelude.drop 2 
            (Prelude.foldl 
                (\acc (key, valor) -> acc ++ ", " ++ key ++ ":" ++ show valor) 
                "" 
                (Data.Map.toList mapa)) 
            ++ "}"


getGlobal :: VarName -> Globals -> Loc
getGlobal nome env = case Data.Map.lookup nome env of
    (Just loc) -> loc
    Nothing -> error $ "Variável de nome " ++ show nome ++ " inexistente no escopo"


--- Obter valores do ambiente de Nomes
get :: VarName -> Env -> Loc

get nome (Stop : _, global) = getGlobal nome global
get nome ([], global) = getGlobal nome global
get nome (Frame frame : env, g) = case Data.Map.lookup nome frame of
    (Just loc) -> loc 
    Nothing -> get nome (env, g)

--- insere valor no ambiente, se já existe, sobrescreve
set :: VarName -> Loc -> Env -> Env

set _ _ (Stop : _, _) = error "Topo da pilha corrompido, contendo Stop." 
set _ _ ([], _) = error "Topo da pilha corrompido, topo vazio." 
set nome loc (Frame frame : env, g) = (Frame (Data.Map.insert nome loc frame) : env, g)

setGlobal :: VarName -> Loc -> Env -> Env
setGlobal nome loc (env, g) = (env, Data.Map.insert nome loc g)

newFrame :: Values 
newFrame = Frame empty

push :: Values -> Env -> Env
push v (env, g) = (v : env, g)

pop :: Env -> (Values, Env)
pop (h : t, g) = (h, (t , g))
pop ([], _) = error "Pilha de ambientes é vazia"

new :: Env
new = ([], empty)