{-# LANGUAGE LambdaCase #-}

module FParser where

import Parser
import FLexer (Token(..))
import PclFront


import Control.Applicative (Alternative (..), liftA2)


satisifies :: (FLexer.Token -> Bool) -> Parser FLexer.Token
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
        funcMain = Main <$> (token FLexer.Let *> token OpenParentesis *> token CloseParentesis *> expr) 
        funcDeclaration = DeclFunc <$> 
            (token FLexer.Let *> funcName) <*> 
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
        composeExp = opsR (PclFront.Comp <$ token FLexer.Semicolon) ifExp

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
unaryExp = notExp <|> derefExp <|> refExp <|> scopeExp <|> factor
    where
        notExp = Not <$> (token Exclamation *> factor)
        derefExp = Deref <$> (token Star *> factor)
        refExp = Ref <$> (token Ampersand *> varName) -- ref só contem nome de var
        scopeExp = Scope <$> (token OpenBrace *> expr <* token CloseBrace)


factor :: Parser Exp
factor = declareExp <|> memCalls <|> nullMacro <|> panicMacro <|> callExp <|> nameExp <|> littExp <|> parensed expr
    where 
        declareExp = Pcl.Let <$> (token Lexer.Let *> varName) <*> (token OpenBracket *> number <* token CloseBracket)
        memCalls = mallocExp <|> freeExp
            where
                mallocExp = Pcl.Malloc <$> (token Lexer.Malloc *> parensed expr)
                freeExp = Pcl.Free <$> 
                    (token Lexer.Free *> token OpenParentesis *> expr) <*> 
                    (token Comma *> expr <* token CloseParentesis)

        nullMacro = Value (Loc nullLoc) <$ token Lexer.Null
        panicMacro = Pcl.Panic UserError <$ token Lexer.Panic

        callExp = CallFunc <$> 
            funcName <*> (parensed (sepBy1 expr (token Comma)) <|> emptyParentesis)
            where emptyParentesis = token OpenParentesis *> token CloseParentesis *> Parser (\list -> [([], list)])

        nameExp = PclFront.Var <$> varName 
        littExp = Value . Number <$> number

-- no artigo, depois da compilação final, aparentemente o resultado é só um elemento,
-- dado que não foi ambiguo
-- no meu caso, ao rodar o parser, para cada ;, o parser pode decidir parar
-- então deve-se filtrar todos os parsers que não terminem. 
-- tenho que ver o que fazer para isso não acontecer

parser :: [Token] -> Either ParseError Function
parser input = case runParser functions input of
    [] -> Left FailedParse
    parses -> let fullparses = filter (\(_, list) -> null list) parses in
        case fullparses of
            [(res,_)] -> Right res
            tooMany ->  Left $ AmbiguousParse tooMany