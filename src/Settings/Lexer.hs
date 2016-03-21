module Lexer (lexrc) where
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many, optional)
import Control.Arrow
import qualified Data.Map.Strict as M
import qualified Data.List as L

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

tokenize :: FilePath -> Either ParseError [Token]
tokenize x = (:) (TSection "general") <$> parse final "kattisrc" x

structure :: [Token] -> M.Map String (M.Map String String)
structure = M.fromList . map (second M.fromList . prepareMap) . L.groupBy issec  

tuple (TKeyVal a b) = (a,b)
prepareMap ((TSection s):xs) = (s,map tuple xs)


issec (TSection _) (TKeyVal _ _) = True
issec _ _ = False

lexrc :: FilePath -> Either ParseError (M.Map String (M.Map String String))
lexrc x = structure <$> tokenize x 
