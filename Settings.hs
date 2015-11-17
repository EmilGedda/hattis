module Settings where
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad
import Data.Maybe
import Control.Applicative

findSettings = liftM3 maybe defval fun (lookupEnv "XDG_CONFIG_HOME") -- Check if $XDG_CONFIG_HOME is defined
            where defval = liftM (</> ".config" </> "kattis" </> "kattisrc") getHomeDirectory -- Default value, ~/.config/kattis/kattisrc
                  fun = return $ (</> "kattis" </> "kattisrc") -- $XDG_CONFIG_HOME/kattis/kattisrc
