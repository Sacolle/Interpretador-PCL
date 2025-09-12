{-# LANGUAGE LambdaCase #-}

module FLexer where

-- Copia meio descarada do Lexer.hs
-- poderia separar as classes e tal, mas muita mão agora, depois eu junto
import Lexer (LexerError(..), unexpected, Lexer(..), satisfies, char, string, oneOf)

import Control.Applicative (Alternative (..), liftA2)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Maybe ()

data Token = 
    NullPtr -- nullptr 
    | NullAlias -- nullptr 
    | Stop -- Stop
    | Exclamation -- ! 
    | Star -- * 
    | Int -- @ 
    | At -- @ 
    | SingleQuote -- '
    | Arrow -- ->
    | Increasse -- +=
    | Decreasse -- -=
    | Ampersand -- & 
    | Semicolon -- ;
    | Colon -- :
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
    | New -- new
    | Delete -- delete
    | Let -- let
    | Var -- var
    | Assign -- :=
    | Alias -- alias
    | AliasDeref -- alias*
    | Swap -- :=:
    | SwapDeref -- :=:*
    | If -- if
    | Else -- else
    | Fn -- fn
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
                    [_] -> Left UnexpectedEOF
                    h1:h2:t  -> if not(h1 == '*' && h2 == '/') then Right (h1, h2 : t)
                        else Left (unexpected (h1:h2:t))

        keyword :: Lexer Token
        keyword = 
            oneOf
                [
                    New <$ string "new",
                    Delete <$ string "delete",
                    Let <$ string "let",
                    Var <$ string "var",
                    NullPtr <$ string "nullptr",
                    NullAlias <$ string "nullalias",
                    Stop <$ string "stop",
                    If <$ string "if",
                    Else <$ string "else",
                    Alias <$ string "alias",
                    AliasDeref <$ string "alias*",
                    Fn <$ string "fn",
                    Int <$ string "int"
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
                    Assign <$ string ":=",
                    At <$ char '@',
                    SingleQuote <$ char '\'',
                    Arrow <$ string "->",
                    Increasse <$ string "+=",
                    Decreasse <$ string "-=",
                    Swap <$ string ":=:",
                    SwapDeref <$ string ":=:*"
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
