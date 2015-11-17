import System.IO
import System.Exit
import GetOpt
import System.Environment

main :: IO ()
main = do argv <- getArgs
          (flags, strs) <- parseOptions argv defaults usage flags Version versionstr Help
          --the flags list contains all flags set, strs is a list of all non flag arguments
          return ()


versionstr = "0.0.0"
usage = "Usage: kattis [-fVvh] [-m FILE] [-l LANG] PROBLEMID FILES\n"

data Flag
    = Force
    | Version
    | Verbose
    | Help
    | MainClass
    | Language
    | Config
    deriving (Eq, Enum, Show)

defaults :: OptionList Flag
defaults = []

flags :: OptionSpecs Flag
flags = makeOptions
    [ (Force,       'f', "force",   noArg, [], "Force submission without confirmation")
    , (Version,     'V', "version", noArg, [], "Displays version information")
    , (Verbose,     'v', "verbose", noArg, [], "Enables verbose output, useful for debugging")
    , (Help,        'h', "help",    noArg, [], "Displays this help message")
    , (MainClass,   'm', "main",    reqArg, "FILE", "Includes and selects FILE as the mainclass")
    , (Language,    'l', "lang",    reqArg, "LANG", "Overrides the default language detection with LANG")
    , (Config,      'c', "conf",    reqArg, "FILE", "The location of the kattisrc file, containing login token and username")
    ]
