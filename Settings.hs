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
import Data.HashMap.Strict
import Data.Text

loginAuth = getSettings >>= readIniFile >>= either (throw . MiscError) return
                >>= return . lookupDefault (throw NoUserSection) (pack "user") . unIni

getSettings = path >>= doesFileExist >>= \case
                True -> path
                False -> throw SettingsNotFound
                where path = settingsLocation

settingsLocation = liftA3 maybe defval fun (lookupEnv "XDG_CONFIG_HOME") -- Is $XDG_CONFIG_HOME defined?
            where defval = (</> ending ".config" ) <$> getHomeDirectory  -- No: ~/.config/kattis/kattisrc
                  fun = return ending -- Yes: $XDG_CONFIG_HOME/kattis/kattisrc
                  ending = (</> "kattis" </> "kattisrc")
