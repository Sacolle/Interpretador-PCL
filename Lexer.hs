{-# LANGUAGE LambdaCase #-}


-- código extraído e modificado de https://github.com/cronokirby/haskell-in-haskell
-- o blog dele é super interessante e recomendo muito para entender o funcionamento
-- do lexer e do parser https://cronokirby.com/posts/2020/12/haskell-in-haskell-2/
-- eu removi os operadores especiais e adaptei o lexer a PCL


module Lexer (Token (..), LexerError (..), lexer, hack) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, filterM)
import Control.Monad.State (State, gets, modify', runState)
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.List (foldl', foldl1')
import Data.Maybe (listToMaybe, maybeToList)
import Data.Bifunctor (first, second)


-- Represents the kind of error that can occur
data LexerError
  = -- We encountered a character we weren't expecting to see
    Unexpected Char
  | -- We reached the end of a file while still expecting characters
    UnexpectedEOF
  deriving (Eq, Show)

-- Create the right lex error when we encounter an unexpected string
unexpected :: String -> LexerError
unexpected [] = UnexpectedEOF
unexpected (c : _) = Unexpected c

-- A Lexer takes an input string, and can consume part of that string to return a result, or fail
--
-- Lexers are like parser combinators, except that they cannot do conditional decision making,
-- or return multiple results. They always return the result that consumed more input,
-- which corresponds to the "longest match" rule you want in a lexical analyzer
newtype Lexer a = Lexer {runLexer :: String -> Either LexerError (a, String)}

-- We can map over the result of a lexer, without changing what strings are recognized
instance Functor Lexer where
  fmap f (Lexer l) = Lexer (fmap (first f) . l)

-- We can squash two lexers together, getting a lexer that recognizes the first input,
-- followed by the second input
instance Applicative Lexer where
  pure a = Lexer (\input -> Right (a, input))
  Lexer lF <*> Lexer lA =
    Lexer $ \input -> do
      (f, rest) <- lF input
      (a, s) <- lA rest
      return (f a, s)

-- We can choose between two successful lexes by picking the one that consumed more input
instance Alternative Lexer where
  empty = Lexer (Left . unexpected)
  Lexer lA <|> Lexer lB =
    Lexer $ \input -> case (lA input, lB input) of
      (res, Left _) -> res
      (Left _, res) -> res
      -- Implement the longest match rule
      (a@(Right (_, restA)), b@(Right (_, restB))) ->
        if length restA <= length restB then a else b

-- A lexer that matches a single character matching a predicate
satisfies :: (Char -> Bool) -> Lexer Char
satisfies p =
  Lexer $ \case
    c : cs | p c -> Right (c, cs)
    rest -> Left (unexpected rest)

-- A lexer that matches a single character
char :: Char -> Lexer Char
char target = satisfies (== target)

-- A lexer that matches an entire string
string :: String -> Lexer String
string = traverse char

-- Create an alternation of a list of lexers.
--
-- This will match if any of the lexers matches, picking the longest match, as usual.
oneOf :: Alternative f => [f a] -> f a
oneOf = foldl1' (<|>)


data Token 
    = As -- as
    | M -- m
    | P -- p
    | Exclamation -- ! 
    | Star -- * -> usado para deref e mult
    | Ampersand -- & -> usado para ref e and
    | Semicolon -- ;
    | Add -- +
    | Sub -- -
    | Less -- <
    | Greater -- >
    | Equal -- =
    | Or -- |
    | OpenBrace -- {
    | CloseBrace -- }
    | OpenBracket -- [
    | CloseBracket -- ]
    | OpenParentesis -- (
    | CloseParentesis -- )
    | Comma -- ,
    | Malloc -- malloc
    | Free -- free
    | Let -- let
    | Assign -- :=
    | If -- if
    | Else -- else
    | While -- While
    | IntLit Int -- qualquer número
    | Name String -- qualquer nome
    | WhiteSpace
    | Comment String
    deriving (Eq, Show)


token :: Lexer Token
token = whiteSpace <|> keyword <|> operators <|> literals <|> names
    where
        whiteSpace :: Lexer Token
        whiteSpace =  blockComment <|> lineComment <|> space
            where
                -- não está funcionando e eu n sei pq
                blockComment = Comment <$> (string "/*" *> many meio <* string "*/")
                lineComment = Comment <$> (string "//" *> many (satisfies (/= '\n')))
                space = WhiteSpace <$ some (satisfies isSpace)
                meio = Lexer $ \case
                    [] -> Left UnexpectedEOF
                    [h1] -> Left UnexpectedEOF
                    h1:h2:t  -> if not(h1 == '*' && h2 == '/') then Right (h1, h2 : t)
                        else Left (unexpected (h1:h2:t))

        keyword :: Lexer Token
        keyword = 
            oneOf
                [
                    As <$ string "as" ,
                    M <$ string "m",
                    P <$ string "p",
                    Malloc <$ string "malloc",
                    Free <$ string "free",
                    Let <$ string "let",
                    If <$ string "if",
                    Else <$ string "else",
                    While <$ string "while"
                ]
        
        operators :: Lexer Token
        operators =
            oneOf
                [
                    Exclamation <$ char '!',
                    Star <$ char '*',
                    Ampersand <$ char '&',
                    Semicolon <$ char ';',
                    Add <$ char '+',
                    Sub <$ char '-',
                    Less <$ char '<',
                    Greater <$ char '>',
                    Equal <$ char '=',
                    Or <$ char '|',
                    OpenBrace <$ char '{',
                    CloseBrace <$ char '}',
                    OpenBracket <$ char '[',
                    CloseBracket <$ char ']',
                    OpenParentesis <$ char '(',
                    CloseParentesis <$ char ')',
                    Comma <$ char ',',
                    Assign <$ string ":="
                ]
        
        literals :: Lexer Token
        literals = intLiterals
            where
                intLiterals = IntLit . read <$> some (satisfies isDigit)
        
        names :: Lexer Token
        names = Name <$> some (satisfies (liftA2 (||) isAlphaNum ('_' ==)))

{-
Lexer [Token] :: String -> Either LexerErro ([Token],String)

token :: Lexer Token

-}
-- poderia se utilizar da expressão (some token) para fazer o parse da lista
-- porém, a função some para quando chegar num erro e devolve o que consegui
-- o que não é o que eu quero, eu quero que se em qualquer momento algo falha,
-- tudo falha
-- Assim a função full complete faz isso, ela loopa sobre a entrada
-- pega 1 token, se falhar colapsa tudo,
-- se não pega até o final da string e retorna Ok quando a string eh vazia
fullComplete :: Lexer [Token]
fullComplete = Lexer $ \s -> loop [] s
    where 
        -- essa função loop, de certa forma, é o lexer, mas mais explicito
        loop :: [Token] -> String -> Either LexerError ([Token], String)
        loop acc str = do 
            (t,str') <- runLexer token str -- se falhar aqui colapsa tudo
            case str' of
                [] -> Right( acc ++ [t], "") 
                _ -> loop (acc ++ [t]) str'-- adiciona o token no fim da lista


lexer :: String -> Either LexerError [Token]
lexer input = runLexer 
    fullComplete input -- roda o input sobre o lexer que gera os tokens
    >>= 
        Right . -- transforma em um option
        filter (\case WhiteSpace -> False; Comment _ -> False; _ -> True) . -- filtra os espaços em branco
        fst -- pega a lista de tokens 

hack input = print (lexer input)
