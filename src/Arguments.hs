module Arguments where
import GetOpt
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
    , (Config,      'c', "conf",    reqArg, "FILE", "Location of kattisrc, with token, username and submissionurl")
    ]
