import Data.Map

data Type = Int | RefT Type deriving (Show)
-- data Adress = AVar Int | ALoc Int
-- data Value = VNum Int | VLoc Int | VRef Adress

type VarName = String
type Number = Int


data Exp = Var VarName
    | Number Number
    | Loc Number
    | Sum Exp Exp
    | Deref Exp
    | Ref Exp
    | Malloc Exp 
    deriving (Show)

data St = Comp St St 
    | Scope St
    | Pop
    | Skip
    | Free Exp Exp
    | Assign Exp Exp
    | Decl VarName Type 
    deriving (Show)


data Mv = Mref VarName | Mloc Number | Mnum Number | MBot | MNull deriving (Show, Eq)

type Pilha = [Map VarName Mv]
type Mem = [Mv]

-- Funções de pilha
pGetMv :: VarName -> Pilha -> Mv
pGetMv x [] = error "valor inexistente na pilha"
pGetMv x (f : p) = case Data.Map.lookup x f of
    (Just v) -> v 
    Nothing -> pGetMv x p

pGet :: VarName -> Pilha -> Exp
pGet x pilha  = case pGetMv x pilha of
    Mref name -> Ref $ Var name
    Mnum n -> Number n
    Mloc l -> Loc l
    _ -> error "acesso a valor inválido na pilha"

pSet :: VarName -> Mv -> Pilha -> Pilha
pSet x v (f : p) = if Data.Map.member x f then Data.Map.insert x v f : p else f : pSet x v p
pSet x v [] = []

--funções de memória
memGetMv :: Int -> Mem -> Maybe Mv
memGetMv _ [] = Nothing
memGetMv 0 (h : t) = Just h
memGetMv l (h : t) = memGetMv (l - 1) t


mvToExp mv = 
    case mv of
        Mnum n -> Number n
        Mloc l -> Loc l 
        Mref name -> Ref $ Var name
        _ -> error "acesso de memória inválido"

memGet :: Int -> Mem -> Exp
memGet l m = maybe (error "Acesso de memória inválido") mvToExp (memGetMv l m)

memInsert :: Int -> Mv -> Mem -> Mem
memInsert 0 mv (_ : t) = mv : t
memInsert i mv (h : t) = h : memInsert (i - 1) mv t
memInsert i mv [] = error "acesso de memória inválido"

-- insere n valores mv apartir de l em mem
memSet :: Int -> Int -> Mv -> Mem -> Mem
memSet 0 0 _ mem = mem    -- finished
memSet 0 n mv (h : t) = mv : memSet 0 (n - 1) mv t -- se chegou no local, começa trocar a cabeça por mv
memSet 0 n mv [] = mv : memSet 0 (n - 1) mv []     -- se chegou no local e não tem nada, insere mv no fim
memSet l n mv (h : t) = h : memSet (l - 1) n mv t  -- se n chegou no local, segue a lista
memSet l n mv [] = MNull : memSet (l - 1) n mv []  -- se chegou ao fim da lista, mas n no local, insere MNull até chegar no local


loc' :: Int -> Int -> Int -> Mem -> Int
loc' count l size m = case memGetMv l m of
    Nothing -> l -- chegou no fim da memória
    Just MNull -> if size == count then l else loc' (count + 1) l size m
    Just _ -> loc' 0 (l + 1) size m

loc :: Int -> Mem -> Int
loc = loc' 0 1



-- Semântica Operacional Small-step de PCL_{mem}

stStep :: (St, Pilha, Mem) -> (St, Pilha, Mem)

-- caso que finaliza a execução da branch
stStep (Skip, p, h) = (Skip, p, h)

-- S-escopo
stStep (Scope s, p, h) = stStep (Comp s Pop, empty : p, h)
-- S-pop
stStep (Pop, f : p, h) = stStep (Skip, p, h)

-- S-Skip
stStep (Comp Skip s2, p, m) = stStep (s2, p, m)
-- S-compose
stStep (Comp s1 s2, p, m) = let (s1', p', m') = stStep (s1, p, m) in
    stStep (Comp s1' s2, p', m')

-- S-declara
stStep (Decl name _, f : p, m) = stStep (Skip, insert name MBot f : p, m)

-- S-free
stStep (Free (Loc l) (Number n), p, m) = stStep (Skip, p, memSet l n MNull m)
-- S-free-step-2
stStep (Free (Loc l) e, p, m) = let (e', p', m') = expStep (e, p, m) in 
    stStep (Free (Loc l) e', p', m')
-- S-free-step-1
stStep (Free e1 e2, p, m) = let (e1', p', m') = expStep (e1, p, m) in 
    stStep (Free e1' e2, p', m')

-- Assing
-- S-atribui-var + s-atribui-var-step
stStep (Assign (Var name) e2, p, m) = 
    case e2 of
        Number n -> if pGetMv name p /= MNull then 
            stStep (Skip, pSet name (Mnum n) p, m) 
            else error (name ++ " não alocada")
        Loc l -> if pGetMv name p /= MNull then 
            stStep (Skip, pSet name (Mloc l) p, m)
            else error (name ++ " não alocada")
        Ref (Var name') -> if pGetMv name p /= MNull then 
            stStep (Skip, pSet name (Mref name') p, m)
            else error (name ++ " não alocada")
        _ -> let (e2', p', m') = expStep (e2, p, m) in 
            stStep (Assign (Var name) e2', p', m') 

-- S-atribui-loc + s-atribui-loc-step
stStep (Assign (Deref (Loc l)) e2, p, m) = 
    case e2 of
        Number n -> if maybe False (/= MNull) (memGetMv l m) then
            stStep (Skip, p, memInsert l (Mnum n) m)
            else error (show l ++ " não alocada")
        Loc l -> if maybe False (/= MNull) (memGetMv l m) then
            stStep (Skip, p, memInsert l (Mloc l) m)
            else error (show l ++ " não alocada")
        Ref (Var name) -> if maybe False (/= MNull) (memGetMv l m) then
            stStep (Skip, p, memInsert l (Mref name) m)
            else error (show l ++ " não alocada")
        _ -> let (e2', p', m') = expStep (e2, p, m) in 
            stStep (Assign (Deref (Loc l)) e2', p', m') 

-- S-atribui-deref-ref
stStep (Assign (Deref (Ref e1)) e2, p, m) = stStep (Assign e1 e2, p, m)

-- S-atribui-step
stStep (Assign (Deref e1) e2, p, m) = let (e1', p', m') = expStep (e1, p, m) in 
    stStep (Assign (Deref e1') e2, p', m')


expStep :: (Exp, Pilha, Mem) -> (Exp, Pilha, Mem)

-- values
expStep (Number n, p, m) = (Number n, p, m)
expStep (Loc l, p, m) = (Loc l, p, m)
expStep (Ref (Var name), p, m) = (Ref (Var name), p, m)

-- E-var
expStep (Var name, p, m) = let v = pGet name p in expStep (v, p, m)

-- E-deref-loc
expStep (Deref (Loc l), p, m) = let v = memGet l m in expStep (v, p, m)
-- E-deref-step
expStep (Deref e, p, m) = let (e', p', m') = expStep (e, p, m) in expStep (Deref e', p', m')

-- E-sum
expStep (Sum (Number n1) (Number n2), p, m) = expStep (Number (n1 + n2), p, m)
expStep (Sum (Number n) (Loc l), p, m) = expStep (Loc (l + n), p, m)
expStep (Sum (Loc l) (Number n), p, m) = expStep (Loc (l + n), p, m)
expStep (Sum (Loc l1) (Loc l2), p, m) = expStep (Loc (l1 + l2), p, m)

-- E-sum-dir
expStep (Sum (Number n1) e, p, m) = let (e', p', m') = expStep (e, p, m) in 
    expStep (Sum (Number n1) e', p', m')
expStep (Sum (Loc l) e, p, m) = let (e', p', m') = expStep (e, p, m) in 
    expStep (Sum (Loc l) e', p', m')

-- E-sum-esq
expStep (Sum e1 e2, p, m) = let (e1', p', m') = expStep (e1, p, m) in 
    expStep (Sum e1' e2, p', m')

-- E-ref-deref
expStep (Ref (Deref e), p, m) = expStep (e, p, m)
-- E-deref-ref
expStep (Deref (Ref e), p, m) = expStep (e, p, m)

-- E-malloc
expStep (Malloc (Number n), p, m) = let l = loc n m in expStep (Loc l, p, memSet l n MBot m)
-- e-malloc-step
expStep (Malloc e, p, m) = let (e', p', m') = expStep (e, p, m) in 
    expStep (Malloc e', p', m')

programPilha = Comp (Decl "x" Int) (
    Comp (Decl "y" (RefT Int)) (
    Comp (Assign (Var "x") (Number 4)) (
    Comp (Assign (Var "y") (Ref (Var "x"))) 
        (Assign (Deref (Var "y")) (Number 2))
    )))

programMem = Comp (Decl "x" (RefT Int)) (
    Comp (Decl "y" (RefT Int)) (
    Comp (Assign (Var "x") (Malloc (Number 2))) (
    Comp (Assign (Deref (Var "x")) (Number 7)) (
    Comp (Assign (Deref (Sum (Var "x") (Number 1))) (Number 3)) (
    Comp (Assign (Var "y") (Sum (Var "x") (Number 1))) 
        (Assign (Deref (Var "y")) (Number 5))
    )))))

programError = Comp (Decl "x" (RefT Int)) (
    Comp (Decl "y" (RefT Int)) (
    Comp (Assign (Var "x") (Malloc (Number 2))) (
    Comp (Assign (Deref (Var "x")) (Number 7)) 
        (Assign (Deref (Sum (Var "x") (Number 3))) (Number 3)))))

programError2 = Assign (Deref (Loc 1)) (Number 2)
main = do 
	{-
    print programMem
    print (stStep (programMem, [empty], []))
	-}
	{-
    print programPilha
    print (stStep ( programPilha, [empty], []))
	-}
    print programError2
    print (stStep ( programError2, [empty], []))

