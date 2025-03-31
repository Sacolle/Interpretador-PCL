{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative (Alternative (..), liftA2)
import Data.List (foldl')
import Lexer (Token (..))
import Data.Bifunctor (first, second)
import Porcelain (FuncName, VarName, Number, Exp(..), Binop(..), Value (Number), Locals (..), Function(..))

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


functions :: Parser Function
functions = funcDeclaration <|> funcMain
    where
        funcMain = Main <$> (token Lexer.Let *> token OpenParentesis *> token CloseParentesis *> expr) 
        funcDeclaration = DeclFunc <$> 
            funcName <*> 
            (parensed (sepBy1 varName (token Comma)) <|> emptyParentesis) <*>
            expr <*>
            functions
            where 
                emptyParentesis = token OpenParentesis *> token CloseParentesis *> Parser (\list -> [([], list)])


expr :: Parser Exp
expr = binExp <|> declareExp <|> assignExp
--ifExp <|> whileExp <|> compExp <|> scopeExp <|> declareExp 


binExp :: Parser Exp
binExp = boolExp
    where 
        boolExp = opsL booleanOperators compExp
            where 
                booleanOperators = 
                    (Binop And <$ token Ampersand) <|> 
                    (Binop Porcelain.Or <$ token Lexer.Or)
        compExp = opsL comparatorOperators addSubExp
            where 
                comparatorOperators = 
                    (Binop Porcelain.Less <$ token Lexer.Less ) <|>
                    (Binop Porcelain.Greater <$ token Lexer.Greater ) <|>
                    (Binop Porcelain.Equal <$ token Lexer.Equal ) 
        addSubExp = opsL (
            (Binop Porcelain.Add <$ token Lexer.Add) <|> 
            (Binop Porcelain.Sub <$ token Lexer.Sub)
            ) multExp
        multExp = opsL (Binop Mult <$ token Star) unaryExp


unaryExp :: Parser Exp
unaryExp = notExp <|> asExp <|> derefExp <|> refExp <|> factor
    where
        notExp = Not <$> (token Exclamation *> factor)

        asExp = Porcelain.As <$> (factor <* token Lexer.As) <*> local
            where local = Parser $ \case
                    t : ts -> case t of
                        Lexer.M -> [(Memoria, ts)]
                        Lexer.P -> [(Pilha, ts)]
                        _ -> []
                    _ -> []
    
        derefExp = Deref <$> (token Star *> factor)
        refExp = Ref <$> (token Ampersand *> varName) -- ref só contem nome de var


factor :: Parser Exp
factor = memCalls <|> callExp <|> nameExp <|> littExp <|> parensed expr
    where 
        memCalls = mallocExp <|> freeExp
            where
                mallocExp = Porcelain.Malloc <$> (token Lexer.Malloc *> parensed expr)
                freeExp = Porcelain.Free <$> 
                    (token Lexer.Free *> token OpenParentesis *> expr) <*> 
                    (token Comma *> expr <* token CloseParentesis)

        callExp = CallFunc <$> 
            funcName <*> (parensed (sepBy1 expr (token Comma)) <|> emptyParentesis)
            where emptyParentesis = token OpenParentesis *> token CloseParentesis *> Parser (\list -> [([], list)])

        nameExp = Var <$> varName 
        littExp = Value . Number <$> number

ifExp :: Parser Exp
ifExp = Porcelain.If <$> 
    (token Lexer.If *> parensed expr) <*> 
    expr <*> 
    (token Lexer.Else *> expr)

whileExp :: Parser Exp
whileExp = Porcelain.While <$> 
    (token Lexer.While *> parensed expr) <*> expr 

compExp :: Parser Exp
compExp = Porcelain.Comp <$> expr <* token Semicolon <*> expr

scopeExp :: Parser Exp
scopeExp = Scope <$> (token OpenBrace *> expr <* token CloseBrace)

declareExp :: Parser Exp
declareExp = Porcelain.Let <$> (token Lexer.Let *> varName) <*> (token OpenBracket *> number <* token CloseBracket)

assignExp :: Parser Exp
assignExp = Porcelain.Assign <$> expr <*> (token Lexer.Assign *> expr)

{-
data ParseError = FailedParse | AmbiguousParse [(Function, [Token])] deriving(Show)

parser :: [Token] -> Either ParseError Function
parser input = case runParser functions input of
    [] -> Left FailedParse
    [(res, _)] -> Right res
    tooMany -> Left (AmbiguousParse tooMany)
-}

--data ParseError = FailedParse | AmbiguousParse [(Exp, [Token])] deriving(Show)

parser = runParser expr