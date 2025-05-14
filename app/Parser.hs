{-# LANGUAGE LambdaCase #-}

module Parser(parser, ParseError) where

import Control.Applicative (Alternative (..), liftA2)
import Data.List (foldl')
import Lexer (Token (..))
import Data.Bifunctor (first)
import Pcl (FuncName, VarName, Number, Exp(..), Binop(..), Value (Number, Loc), Loc(Memoria), Function(..), Global(..))

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


globals :: Parser Global
globals = globalDeclaration <|> firstFunction
    where
        firstFunction = Func <$> functions
        globalDeclaration = DeclGlobal <$> 
            (token Lexer.Global *> varName) <*> 
            (token OpenBracket *> number <* token CloseBracket) <*>
            globals

functions :: Parser Function
functions = funcDeclaration <|> funcMain
    where
        funcMain = Main <$> (token Lexer.Let *> token OpenParentesis *> token CloseParentesis *> expr) 
        funcDeclaration = DeclFunc <$> 
            (token Lexer.Let *> funcName) <*> 
            (parensed (sepBy1 varName (token Comma)) <|> emptyParentesis) <*>
            expr <*>
            functions
            where 
                emptyParentesis = token OpenParentesis *> token CloseParentesis *> Parser (\list -> [([], list)])


expr :: Parser Exp
expr = binExp


binExp :: Parser Exp
binExp = composeExp
    where 
        composeExp = opsR (Pcl.Comp <$ token Lexer.Semicolon) whileExp
        --here

        -- if causa problema de ambiguidade
        -- if (1) 1 else 0;3 deve ser ter o parse (if (1) (1) else 0);3
        -- mas atualmente ele também realiza if (1) (1) else (0;3)
        whileExp = (Pcl.While <$> (token Lexer.While *> parensed whileExp) <*> whileExp) <|> ifExp

        ifExp = (Pcl.If <$> 
            (token Lexer.If *> parensed ifExp) <*>
            ifExp <*> 
            (token Lexer.Else *> ifExp)
            ) <|> assignExp

        assignExp = opsL (Pcl.Assign <$ token Lexer.Assign) boolExp
        boolExp = opsL booleanOperators compExp
            where 
                booleanOperators = 
                    (Binop And <$ token Ampersand) <|> 
                    (Binop Pcl.Or <$ token Lexer.Or)
        compExp = opsL comparatorOperators addSubExp
            where 
                comparatorOperators = 
                    (Binop Pcl.Less <$ token Lexer.Less ) <|>
                    (Binop Pcl.Greater <$ token Lexer.Greater ) <|>
                    (Binop Pcl.Equal <$ token Lexer.Equal ) 
        addSubExp = opsL (
            (Binop Pcl.Add <$ token Lexer.Add) <|> 
            (Binop Pcl.Sub <$ token Lexer.Sub)
            ) multExp
        multExp = opsL (Binop Mult <$ token Star) unaryExp


unaryExp :: Parser Exp
unaryExp = notExp <|> derefExp <|> refExp <|> scopeExp <|> factor
    where
        notExp = Not <$> (token Exclamation *> factor)
        derefExp = Deref <$> (token Star *> factor)
        refExp = Ref <$> (token Ampersand *> varName) -- ref só contem nome de var
        scopeExp = Scope <$> (token OpenBrace *> expr <* token CloseBrace)


factor :: Parser Exp
factor = declareExp <|> memCalls <|> callExp <|> nullMacro <|> nameExp <|> littExp <|> parensed expr
    where 
        declareExp = Pcl.Let <$> (token Lexer.Let *> varName) <*> (token OpenBracket *> number <* token CloseBracket)
        memCalls = mallocExp <|> freeExp
            where
                mallocExp = Pcl.Malloc <$> (token Lexer.Malloc *> parensed expr)
                freeExp = Pcl.Free <$> 
                    (token Lexer.Free *> token OpenParentesis *> expr) <*> 
                    (token Comma *> expr <* token CloseParentesis)

        callExp = CallFunc <$> 
            funcName <*> (parensed (sepBy1 expr (token Comma)) <|> emptyParentesis)
            where emptyParentesis = token OpenParentesis *> token CloseParentesis *> Parser (\list -> [([], list)])

        nullMacro = Value (Loc (Memoria 0)) <$ token Lexer.Null
        nameExp = Var <$> varName 
        littExp = Value . Number <$> number

-- no artigo, depois da compilação final, aparentemente o resultado é só um elemento,
-- dado que não foi ambiguo
-- no meu caso, ao rodar o parser, para cada ;, o parser pode decidir parar
-- então deve-se filtrar todos os parsers que não terminem. 
-- tenho que ver o que fazer para isso não acontecer

data ParseError = FailedParse | AmbiguousParse [(Global, [Token])] deriving(Show)

parser :: [Token] -> Either ParseError Global
parser input = case runParser globals input of
    [] -> Left FailedParse
    parses -> let fullparses = filter (\(_, list) -> null list) parses in
        case fullparses of
            [(res,_)] -> Right res
            tooMany ->  Left $ AmbiguousParse tooMany