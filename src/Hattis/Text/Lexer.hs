module Hattis.Ini where
import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad (void)
import Data.Bifunctor
import Hattis.Error
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

data Token
    = TKeyVal { key :: String, value :: String}
    | TSection { section :: String, keyvalues :: [Token]}
    deriving Show

-- space consumer
sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "#") empty

symbol :: String -> Parser String
symbol = L.symbol sc

sections :: Parser Token
sections = TSection <$> between (char '[') (symbol "]") (some letterChar)
                   <*> many keyvals

keyvals :: Parser Token
keyvals = TKeyVal <$> someTill letterChar (string ": ") <*> someTill anyChar eol

final :: Parser [Token]
final = sc *> many (sections <* sc) <* eof

tokenize :: String -> Either String [Token]
tokenize = first ((++) "Error while parsing kattisrc: \n" . show) 
                . parse final "kattisrc" 

class IniStorage a where
    keyinsection :: a -> String -> String -> Maybe String
