module Settings (loadSettings, Setting(..), SettingsStorage) where
import System.Environment
import System.Directory
import System.FilePath
import Error
import Control.Applicative
import Control.Monad
import Settings.Parser
import Data.Bool

loadSettings :: String -> KattisApp SettingsStorage
loadSettings [] = wrapKattis . parse =<< joinIO (exist <$> location)
loadSettings s  = wrapKattis . parse =<< exist s

exist :: FilePath -> KattisApp FilePath 
exist = (wrapKattis .) . fmap . bool (Left SettingsNotFound) . Right <*> doesFileExist 

location = liftM3 maybe defval fun (lookupEnv "XDG_CONFIG_HOME")
            where defval = (</> ending ".config") <$> getHomeDirectory
                  fun    = return ending
                  ending = (</> "hattis" </> "kattisrc")
