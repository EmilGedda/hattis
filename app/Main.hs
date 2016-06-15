import Hattis.GetOpt
import Hattis.Arguments
import System.Environment

main :: IO ()
main = do argv <- getArgs
          (flagsset, id:files) <- parseOptions argv defaults usage flags Version versionstr Help
--          let auth = getauth flagsset
--          let lang = verifyfiles files
          putStr "End of line."
--          --the flags list contains all flags set, strs is a list of all non flag arguments
--
--getauth :: OptionList Flag -> KattisApp FilePath
--getauth flg = joinIO (settingsExist . getOptionOr Config flg <$> settingsLocation)
--
versionstr = "0.1.0\n"
usage = "Usage: kattis [-fvhsV] [-c KATTISRC] [-m FILE] [-l LANG] PROBLEMID FILES ...\n"
