module Hattis.Ini where
import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad (void)
import Data.Bifunctor
import Data.Map hiding (empty, map)
import Hattis.Error
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L


-- Below is the parser section
data Token
    = TKeyVal String String
    | TSection String [Token] 
    deriving Show

-- space consumer
sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "#") empty

sections' :: Parser Token
sections' = TSection <$> between (char '[') (L.symbol sc "]") (some letterChar)
                     <*> many keyvals'

keyvals' :: Parser Token
keyvals' = TKeyVal <$> someTill letterChar (string ": ") <*> someTill anyChar eol

tokenize :: String -> Either String [Token]
tokenize = first ((++) "Error while parsing kattisrc: \n" . show) 
                . parse (sc *> many (sections' <* sc) <* eof)  "kattisrc" 

-- Here starts the implementation of a Ini settings storage
newtype MapStorage a = MapStorage { getStorage :: Map String (Map String a)}

toStorage :: [Token] -> MapStorage String
toStorage = MapStorage . fromList . map (second (fromList . map tupKV) . tupS)
        where tupS  (TSection a b) = (a,b)
              tupKV (TKeyVal  a b) = (a,b)

data Setting
    = Username
    | Token
    | LoginUrl
    | SubmissionUrl
    deriving Show

class IniMapping a where
    section :: a -> String
    key     :: a -> String

class Ini i where 
    getsetting :: i -> Setting -> Either String a
    keyvalues  :: i -> [(String, [(Setting, a)])]

    keys       :: i -> [Setting]
    keys       = extr fst 
    values     :: i -> [a]
    values     = extr snd 

extr :: Ini i => ((Setting, a) -> b) -> i -> [b]
extr f x = fmap f . snd =<< keyvalues x

instance IniMapping Setting where
    section Username      = "user"
    section Token         = "user"
    section LoginUrl      = "kattis"
    section SubmissionUrl = "kattis"

    key Username          = "username"
    key Token             = "token"
    key LoginUrl          = "loginurl"
    key SubmissionUrl     = "submissionurl"

-- TODO: this
instance Ini (MapStorage a) where
    getsetting = undefined
    keyvalues  = undefined
