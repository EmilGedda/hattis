module Settings where
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad
import Data.Maybe
import Control.Applicative

findSettings = liftM3 maybe defval fun (lookupEnv "XDG_CONFIG_HOME") -- Is $XDG_CONFIG_HOME defined?
            where defval = liftM (</> ".config" </> ending) getHomeDirectory -- No: ~/.config/kattis/kattisrc
                  fun = return (</> ending) -- Yes: $XDG_CONFIG_HOME/kattis/kattisrc
                  ending = "kattis" </> "kattisrc"
