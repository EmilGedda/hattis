module Settings where
import System.Environment
import System.Directory
import System.FilePath
import Control.Exception
import Control.Arrow
import Error
import Control.Applicative
import Control.Monad
import Data.Ini
import Data.List
import Data.Bool
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

loginAuth = fmap ((bool (throw MalformedSettings) id =<<               -- 4. If not, throw else return
                flip all neededvalues . flip elem . map fst) .         -- 3. Check if we have it all
                concatMap M.toList . M.elems) . liftM                  -- 2. Flatten to [(key, value)]
                (either (throw . MiscError) unIni) . readIniFile       -- 1. Try parsing the Ini
 
settingsExist = fmap . bool (throw SettingsNotFound) <*> doesFileExist

settingsLocation = liftM3 maybe defval fun (lookupEnv "XDG_CONFIG_HOME")    -- Is $XDG_CONFIG_HOME defined?
                where defval = (</> ending ".config") <$> getHomeDirectory  -- No: ~/.config/kattis/kattisrc
                      fun    = return ending                                -- Yes: $XDG_CONFIG_HOME/kattis/kattisrc
                      ending = (</> "kattis" </> "kattisrc")

neededvalues = map T.pack ["username", "submissionurl", "token"]
