module Funcs(Funcs, FuncBody, get, set, new) where

import Data.Map ( Map, insert, lookup, empty)
import Pcl (VarName, FuncName, Exp)
--- Ambiente de Funções
type FuncBody = ([VarName], Exp)
type Funcs = Map FuncName FuncBody

--- Obtem o corpo e args da função
get :: FuncName -> Funcs -> FuncBody

get nome funcs = case Data.Map.lookup nome funcs of
    (Just body) -> body
    Nothing -> error "Nome de função inexistente no programa"

--- insere o corpo e args da função no ambiente
set :: FuncName -> FuncBody -> Funcs -> Funcs

set = Data.Map.insert 

new :: Map FuncName FuncBody
new = empty