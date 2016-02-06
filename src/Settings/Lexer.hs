module Lexer where
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many, optional)

data Token
    = TComment String
    | TKeyVal  String String
    | TSection String
    deriving Show

comment :: CharParser () Token
comment = char '#' *> (TComment <$> manyTill anyChar eol)

group :: CharParser () Token 
group = TSection <$> between (char '[') (char ']') (many1 letter)

keyval :: CharParser () Token
keyval = TKeyVal <$> many1 letter <* string ": " <*> manyTill anyChar eol

eol :: CharParser () ()
eol = (comment *> return ()) <|> (optional (char '\r') *> newline *> return ())

final = many $ many eol *> (keyval <|> group)

tokenize :: String -> Either ParseError [Token]
tokenize = parse final "kattisrc"
