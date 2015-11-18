{-# LANGUAGE LambdaCase #-}
module Settings where
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad
import Data.Maybe
import Control.Applicative
import Control.Exception
import Error

getSettings = let path = findSettings in path >>= doesFileExist >>= \case
                 True -> path
                 False -> throw SettingsNotFound


findSettings = liftM3 maybe defval fun (lookupEnv "XDG_CONFIG_HOME") -- Is $XDG_CONFIG_HOME defined?
            where defval = liftM (</> ".config" </> ending) getHomeDirectory -- No: ~/.config/kattis/kattisrc
                  fun = return (</> ending) -- Yes: $XDG_CONFIG_HOME/kattis/kattisrc
                  ending = "kattis" </> "kattisrc"
