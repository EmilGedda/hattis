{-# LANGUAGE FlexibleContexts #-}
module Hattis.Text.SourceFile (langs, FileExt, Language, name, verifyfiles, fromStr) where
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Either
import System.Directory
import Hattis.Error
import System.FilePath
import qualified Data.List as L

type Files = [String]

data Language
    = C
    | CSharp
    | Cpp
    | Go
    | Haskell
    | Java
    | JavaScript
    | ObjectiveC
    | PHP
    | Prolog
    | Python3
    | Ruby
    deriving (Show, Eq, Enum, Ord)

class FileExt a where
    exts :: a -> [String]
    name :: a -> String

instance FileExt Language where
    exts C          = [".c", ".h"]
    exts CSharp     = [".cs"]
    exts Cpp        = [".cpp", ".cc", ".cxx", ".hpp", ".h"]
    exts Go         = [".go"]
    exts Haskell    = [".hs"]
    exts Java       = [".java"] --Todo: Find mainclass
    exts JavaScript = [".js"]
    exts ObjectiveC = [".m", ".h"]
    exts PHP        = [".php"]
    exts Prolog     = [".pl", ".prolog"]
    exts Python3    = [".py"]
    exts Ruby       = [".rb"]

    name CSharp     = "C#"
    name Cpp        = "C++"
    name ObjectiveC = "Objective-C"
    name Python3    = "Python 3"
    name x          = show x


fromStr :: (MonadError HattisError m) => String -> m Language
fromStr x = case filter ((==lower) . map toLower . snd) lang of
                [] -> throwError $ UnknownLanguage x
                (a,_):_ -> return a
            where lang  = map (id &&& name) langs
                  lower = map toLower x

langs :: [Language]
langs = enumFrom C

matches :: FileExt a => String -> a -> Bool
matches = (. exts) . elem

allfiles :: (MonadIO m1, MonadError HattisError m) => Files -> m1 (m Files)
allfiles x = liftIO $ do
        full <- mapM getFullPath x
        existing <- mapM doesFileExist full
        let nonexisting = filter ((False==) . snd) $ zip x existing
        return $ case nonexisting of
            [] -> return full
            l -> throwError . NotAFile $ map fst l

possiblelangs :: String -> [Language]
possiblelangs = flip filter langs . matches

getlangs :: (MonadError HattisError m) => Files -> m [[Language]]
getlangs = mapM $ fun . (id &&& (possiblelangs . takeExtension))
        where fun (f, []) = throwError $ UnknownExtension f
              fun (_, l)  = return l

decidelang :: (MonadError HattisError m) => Files -> m Language
decidelang x = mul . (flip filter langs . possible) . join zip =<< getlangs x
        where possible a b = all (elem b . snd) a
              mul [a] = return a
              mul  a  = throwError . MultipleLanguages . map name $ a

verifyfiles :: (MonadError HattisError m, MonadIO mio) => Files -> mio (m Language)
verifyfiles = liftIO . fmap (decidelang =<<) . allfiles

getFullPath :: String -> IO String
getFullPath s = case splitPath s of
                    "~/" : t -> joinPath . (: t) <$> getHomeDirectory
                    _ -> return s
