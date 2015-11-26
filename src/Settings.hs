module Settings (loginAuth, settingsLocation, settingsExist) where
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
import qualified Data.Ini as I
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

-- Returns the [(key, value)] of all fields in the .ini in FilePath
loginAuth :: FilePath -> IO [(T.Text, T.Text)] 
loginAuth = liftM (settingsValid . concatMap M.toList . M.elems) . parseIni
 
-- Verify that neededvalues is a subset of parsed settings
settingsValid :: [(T.Text, a)] -> [(T.Text, a)]
settingsValid = bool (throw MalformedSettings) id =<< subset neededvalues . map fst
                  where subset = flip $ all . flip elem

-- Try parsing the specified config file, kattisrc
parseIni = liftM (either (throw . MiscError) I.unIni) . I.readIniFile

settingsExist = liftM . bool (throw SettingsNotFound) <*> doesFileExist

settingsLocation = liftM3 maybe defval fun (lookupEnv "XDG_CONFIG_HOME")    -- Is $XDG_CONFIG_HOME defined?
                where defval = (</> ending ".config") <$> getHomeDirectory  -- No: ~/.config/kattis/kattisrc
                      fun    = return ending                                -- Yes: $XDG_CONFIG_HOME/kattis/kattisrc
                      ending = (</> "kattis" </> "kattisrc")

neededvalues = map T.pack ["username", "submissionurl", "token"]
