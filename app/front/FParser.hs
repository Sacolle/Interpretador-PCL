{-# LANGUAGE LambdaCase #-}

module FParser where

import Control.Applicative (Alternative (..), liftA2)
import Data.List (foldl')
import FLexer (Token (..))
import Data.Bifunctor (first)
import PclFront (FuncName, VarName, Number, Exp(..), Binop(..), Value (Number), Function(..), Tipo(..))

-- NOTE: tentar trocar esse parser para
-- Parser {runParser :: [Token] -> [Either ParserError (a, [Token])]}
-- Desse jeito dá para encontrar onde que fica o erro de parse
newtype Parser a = Parser {runParser :: [Token] -> [(a, [Token])]} 

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure a = Parser (\input -> [(a, input)])
  Parser lF <*> Parser lA =
    Parser $ \input -> do
      (f, rest) <- lF input
      (a, s) <- lA rest
      return (f a, s)

instance Alternative Parser where
  empty = Parser (const [])
  Parser lA <|> Parser lB =
    Parser $ \input -> lA input ++ lB input

satisifies :: (Token -> Bool) -> Parser Token
satisifies p =
  Parser $ \case
    t : ts | p t -> [(t, ts)]
    _ -> []

pluck :: (Token -> Maybe a) -> Parser a
pluck f =
  Parser $ \case
    t : ts -> case f t of
      Just res -> [(res, ts)]
      _ -> []
    _ -> []

token :: Token -> Parser Token
token = satisifies . (==)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))

-- tem uma descrição mais extensa da funcionalidade, mas essencialmente
-- essa função recebe o separador e o parser
-- ela primeiro pega o primeiro elemento da sequência p e depois gera a maior lista possivel de
-- tuplas separadores elemento. Com isso, ele merge eles com a operação do sep
opsL :: Parser (a -> a -> a) -> Parser a -> Parser a
opsL sep p = liftA2 squash p (many (liftA2 (,) sep p))
  where
    squash = foldl' (\acc (combine, a) -> combine acc a)

opsR :: Parser (a -> a -> a) -> Parser a -> Parser a
opsR sep p = liftA2 squash p (many (liftA2 (,) sep p))
  where
    squash start annotated =
      let (start', annotated') = foldl' shift (start, []) annotated
          shift (oldStart, stack) (combine, a) = (a, (combine, oldStart) : stack)
       in foldl' (\acc (combine, a) -> combine a acc) start' annotated'



varName :: Parser VarName
varName = 
    pluck $ \case 
    Name name -> Just name
    _ -> Nothing

funcName :: Parser FuncName
funcName = 
    pluck $ \case 
    Name name -> Just name
    _ -> Nothing

number :: Parser Number
number = 
    pluck $ \case 
    IntLit num -> Just num
    _ -> Nothing

parensed :: Parser a -> Parser a
parensed p = token OpenParentesis *> p <* token CloseParentesis

emptyParentesis :: Parser [a]
emptyParentesis = token OpenParentesis *> token CloseParentesis *> Parser (\list -> [([], list)])

tipo :: Parser Tipo
tipo = base <|>  ponteiro <|> alias 
    where
        base = Num <$ token FLexer.Int
        ponteiro = Ptr <$> (token FLexer.Star *> tipo)
        alias = AliasT <$> (token FLexer.At *> tipo) <*> (token FLexer.SingleQuote *> number)

functions :: Parser Function
functions = funcDeclaration <|> funcMain
    where
        funcMain = Main <$> (token FLexer.Fn *> token OpenParentesis *> token CloseParentesis *> expr) 
        funcDeclaration = DeclFunc <$> 
            (token FLexer.Fn *> funcName) <*> 
            (parensed (sepBy1 args (token Comma)) <|> emptyParentesis) <*> 
            (token Arrow *> tipo) <*>
            expr <*>
            functions
            where 
                args = (,) <$> varName <*> tipo

expr :: Parser Exp
expr = binExp


binExp :: Parser Exp
binExp = composeExp
    where 
        composeExp = opsR (PclFront.Comp <$ token FLexer.Semicolon) ifExp
        --here

        ifExp = (PclFront.If <$> 
            (token FLexer.If *> parensed ifExp) <*>
            ifExp <*> 
            (token FLexer.Else *> ifExp)
            ) <|> assignExp

        assignExp = opsL (PclFront.Assign <$ token FLexer.Assign) boolExp
        boolExp = opsL booleanOperators compExp
            where 
                booleanOperators = 
                    (Binop And <$ token Ampersand) <|> 
                    (Binop PclFront.Or <$ token FLexer.Or)
        compExp = opsL comparatorOperators addSubExp
            where 
                comparatorOperators = 
                    (Binop PclFront.Less <$ token FLexer.Less ) <|>
                    (Binop PclFront.Greater <$ token FLexer.Greater ) <|>
                    (Binop PclFront.Equal <$ token FLexer.Equal ) 
        addSubExp = opsL (
            (Binop PclFront.Add <$ token FLexer.Add) <|> 
            (Binop PclFront.Sub <$ token FLexer.Sub)
            ) multExp
        multExp = opsL (Binop Mult <$ token Star) unaryExp


unaryExp :: Parser Exp
unaryExp = notExp <|> derefExp <|> refExp <|> aliasExp <|> aliasDerefExp <|> scopeExp <|> factor
    where
        notExp = Not <$> (token Exclamation *> factor)
        derefExp = Deref <$> (token Star *> factor)
        refExp = Ref <$> (token Ampersand *> varName) -- ref só contem nome de var
        aliasExp = PclFront.Alias <$> (token FLexer.Alias *> varName) -- ref só contem nome de var
        aliasDerefExp = PclFront.AliasDeref <$> (token FLexer.AliasDeref *> varName) -- ref só contem nome de var
        scopeExp = Scope <$> (token OpenBrace *> expr <* token CloseBrace)


factor :: Parser Exp
factor = namedBinOps <|> declareExp <|> memCalls <|> stop <|> nullptr <|> nullalias <|> callExp <|> nameExp <|> littExp <|> parensed expr
    where 
        namedBinOps = increasse <|> decreasse <|> swap <|> swapDeref
            where
                increasse = PclFront.Inc <$> varName <*> (token FLexer.Increasse *> expr)
                decreasse = PclFront.Dec <$> varName <*> (token FLexer.Decreasse *> expr)
                swap = PclFront.Swap <$> varName <*> (token FLexer.Swap *> varName)
                swapDeref = PclFront.SwapDeref <$> varName <*> (token FLexer.SwapDeref *> varName)

        declareExp = PclFront.LetVar <$> (token FLexer.Var *> varName) <*> (token Colon *> tipo <* token FLexer.Assign) <*> expr
        memCalls = mallocExp <|> freeExp
            where
                mallocExp = PclFront.New <$> (token FLexer.Let *> varName) <*> (token Colon *> tipo <* token FLexer.Equal) <*> (token FLexer.New *> parensed expr)
                freeExp = PclFront.Delete <$> 
                    (token FLexer.Delete *> token OpenParentesis *> varName) <*> 
                    (token Comma *> expr <* token CloseParentesis)

        stop = PclFront.Stop <$ token FLexer.Stop
        nullptr = PclFront.NullPtr <$> (token FLexer.NullPtr *> token FLexer.Less *> tipo <* token FLexer.Greater)
        nullalias = PclFront.NullAlias <$> (token FLexer.NullAlias *> token FLexer.Less *> tipo <* token FLexer.Greater)

        callExp = CallFunc <$> 
            (token FLexer.Let *> varName) <*>
            (token FLexer.Colon *> tipo <* token FLexer.Equal) <*>
            funcName <*> (parensed (sepBy1 expr (token Comma)) <|> emptyParentesis)

        nameExp = PclFront.Var <$> varName 
        littExp = Value . Number <$> number

-- no artigo, depois da compilação final, aparentemente o resultado é só um elemento,
-- dado que não foi ambiguo
-- no meu caso, ao rodar o parser, para cada ;, o parser pode decidir parar
-- então deve-se filtrar todos os parsers que não terminem. 
-- tenho que ver o que fazer para isso não acontecer

data ParseError = FailedParse | AmbiguousParse [(Function, [Token])] deriving(Show)

parser :: [Token] -> Either ParseError Function
parser input = case runParser functions input of
    [] -> Left FailedParse
    parses -> let fullparses = filter (\(_, list) -> null list) parses in
        case fullparses of
            [(res,_)] -> Right res
            tooMany ->  Left $ AmbiguousParse tooMany