module Parser where
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)

data Token
    = Section String
    | KeyVal String String
    | Comment String
    deriving Show

group :: CharParser () Token 
group = Section <$> between (char '[') (char ']') (many1 letter) <* newline

comment :: CharParser () Token
comment = char '#' *> (Comment <$> manyTill anyChar newline)

keyval :: CharParser () Token
keyval = KeyVal <$> many1 letter <* string ": " <*> manyTill anyChar newline 

final = many $ sepBy1 (keyval <|> group <|> comment) newline

tokenize :: String -> Either ParseError [Token]
tokenize file = concat <$> parse final "kattisrc" file 
