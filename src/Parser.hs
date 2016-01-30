module Parser where
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)

data Token
    = Comment String
    | KeyVal  String String
    | Section String
    deriving Show

comment :: CharParser () Token
comment = char '#' *> (Comment <$> manyTill anyChar newline)

group :: CharParser () Token 
group = Section <$> between (char '[') (char ']') (many1 letter)

keyval :: CharParser () Token
keyval = KeyVal <$> many1 letter <* string ": " <*> manyTill anyChar newline  

final = many $ many comment *> many newline *> choice [keyval, group]

tokenize :: String -> Either ParseError [Token]
tokenize = parse final "kattisrc"
