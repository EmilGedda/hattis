{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
module Hattis.Text.Ini(Setting(..), IniStorage, IniMapping, Ini(..), loadSettings) where
import Control.Applicative hiding ((<|>), many, optional)
import Control.Arrow hiding (first, second)
import Control.Monad (void)
import Data.Bifunctor 
import qualified Data.Map.Strict as M
import Hattis.Error
import Text.Megaparsec
import Text.Megaparsec.String
import System.Directory
import System.Environment
import System.FilePath
import qualified Text.Megaparsec.Lexer as L
import Control.Concurrent

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

tokenize :: (MonadError HattisError m) => String -> m [Token]
tokenize = either (throwError . ParseFail . show) return 
           . parse (sc *> many (sections' <* sc) <* eof)  "kattisrc" 

exist :: (MonadIO mio, MonadError HattisError merr) => FilePath -> mio (merr String)
exist x = liftIO $ do 
        e <- doesFileExist x
        case e of
            True -> liftM return $ readFile x
            False -> return $ throwError SettingsNotFound

location :: IO FilePath
location = liftM3 maybe defval fun (lookupEnv "XDG_CONFIG_HOME")
            where defval = (</> ending ".config") <$> getHomeDirectory
                  fun    = return ending
                  ending = (</> "hattis" </> "kattisrc")

-- TODO: clean this up
loadSettings :: (MonadIO mio, MonadError HattisError merr) => String -> mio (merr (IniStorage String))
loadSettings [] = liftIO $ (fun =<<) <$> (exist =<< location) 
                where fun x = toStorage <$> tokenize x
loadSettings s  = liftIO $ (fun =<<) <$> exist s 
                where fun x = toStorage <$> tokenize x

-- debug stuff
print' :: Either HattisError [Token] -> IO ()
print' (Left x) = (putStrLn . show) x
print' (Right t) = mapM_ (putStrLn . show) t

newtype IniStorage a = IniStorage { getStorage :: M.Map String (M.Map String a) }

toStorage :: [Token] -> IniStorage String
toStorage = IniStorage . M.fromList . map (second (M.fromList . map tupKV) . tupS)
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

class Ini i a | i -> a where 
    getsetting :: (MonadError HattisError m, IniMapping s) => s -> i -> m a
    keyvalues  :: i -> [(String, [(String, a)])]

    keys       :: i -> [String]
    keys       = extr fst 
    values     :: i -> [a]
    values     = extr snd 

extr :: Ini i a => ((String, a) -> b) -> i -> [b]
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

instance Ini (IniStorage a) a where
    getsetting set stor = fun $ M.lookup (key set) =<< 
                                M.lookup (section set) (getStorage stor)
        where fun (Just x) = return x
              fun Nothing  = throwError $ ErroneousSettings (section set) (key set)

    keyvalues = map (second M.toList) . M.toList . getStorage 
