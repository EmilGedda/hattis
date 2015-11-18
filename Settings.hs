{-# LANGUAGE LambdaCase #-}
module Settings where
import System.Environment
import System.Directory
import System.FilePath
import Control.Exception
import Control.Applicative
import Control.Arrow
import Error
import Control.Monad
import Data.Ini
import Data.List
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

loginAuth = getSettings >>= readIniFile >>= either (throw . MiscError) return >>=
                return . (isValid =<< flip all neededvalues . flip elem . map fst)
                . concatMap M.toList . M.elems . unIni -- Flatten to [(key,values)]
                -- ^Make sure we have the necessary key-values
                where isValid False x = throw MalformedSettings
                      isValid True  x = x
                      -- Custom assert helper

getSettings = path >>= doesFileExist >>= \case
                True  -> path
                False -> throw SettingsNotFound
                where path = settingsLocation

settingsLocation = liftA3 maybe defval fun (lookupEnv "XDG_CONFIG_HOME") -- Is $XDG_CONFIG_HOME defined?
            where defval = (</> ending ".config" ) <$> getHomeDirectory  -- No: ~/.config/kattis/kattisrc
                  fun = return ending -- Yes: $XDG_CONFIG_HOME/kattis/kattisrc
                  ending = (</> "kattis" </> "kattisrc")

neededvalues = map T.pack ["username", "submissionurl", "token"]
