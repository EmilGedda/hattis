module Settings (Auth, loginAuth, settingsLocation, settingsExist) where
import System.Environment
import System.Directory
import System.FilePath
import Control.Exception
import Control.Arrow
import Error
import Control.Applicative
import Control.Monad
import Data.List
import Data.Bool
import Data.Ord
import Data.Function
import Control.Monad.Except
import qualified Data.Ini as I
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

data Auth = Auth {submissionurl :: T.Text, token :: T.Text, username :: T.Text} deriving(Show)

-- Returns the [(key, value)] of all fields in the .ini in FilePath
loginAuth :: FilePath -> KattisApp Auth 
loginAuth = wrapKattis . fmap (fmap (auth . sort) . join . fmap (settingsValid . concatMap M.toList . M.elems)) . parseIni
            where auth [(_,a),(_,b),(_,c)] = Auth a b c -- TODO: improve

-- Verify that neededvalues is a subset of parsed settings
settingsValid :: [(T.Text, a)] -> Either KattisError [(T.Text, a)]
settingsValid s = bool (Left MalformedSettings) (Right filtered) check  
            where filtered = filter (flip elem neededvalues . fst) s
                  check    = all (flip elem $ map fst s) neededvalues

-- Try parsing the specified config file, kattisrc
parseIni = fmap (either (Left . MiscError) (Right . I.unIni)) . I.readIniFile

settingsExist :: FilePath -> KattisApp FilePath 
settingsExist = (wrapKattis .) . fmap . bool (Left SettingsNotFound) . Right <*> doesFileExist 

settingsLocation = liftM3 maybe defval fun (lookupEnv "XDG_CONFIG_HOME")-- Is $XDG_CONFIG_HOME defined?
            where defval = (</> ending ".config") <$> getHomeDirectory  -- No: ~/.config/kattis/kattisrc
                  fun    = return ending                                -- Yes: $XDG_CONFIG_HOME/kattis/kattisrc
                  ending = (</> "kattis" </> "kattisrc")

neededvalues = map T.pack ["username", "submissionurl", "token"]
