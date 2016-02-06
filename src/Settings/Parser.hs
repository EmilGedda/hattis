module Parser where
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many, optional)

data Token
    = Comment String
    | KeyVal  String String
    | Section String
    deriving Show

comment :: CharParser () Token
comment = char '#' *> (Comment <$> manyTill anyChar eol)

group :: CharParser () Token 
group = Section <$> between (char '[') (char ']') (many1 letter)

keyval :: CharParser () Token
keyval = KeyVal <$> many1 letter <* string ": " <*> manyTill anyChar eol

eol :: CharParser () ()
eol = (comment *> return ()) <|> (optional (char '\r') *> newline *> return ())

final = many $ many eol *> (keyval <|> group)

tokenize :: String -> Either ParseError [Token]
tokenize = parse final "kattisrc"
