module Hattis.Arguments where
import Hattis.GetOpt
data Flag
    = Config
    | Force
    | Help
    | Language
    | MainClass
    | Silent
    | Verbose
    | Version
    deriving (Eq, Enum, Show)

defaults :: OptionList Flag
defaults = []

flags :: OptionSpecs Flag
flags = makeOptions
    [ (Config,    'c', "conf",    reqArg, "FILE", "Location of kattisrc, with token, username and submissionurl")
    , (Force,     'f', "force",   noArg, [], "Force submission without confirmation")
    , (Help,      'h', "help",    noArg, [], "Displays this help message")
    , (Language,  'l', "lang",    reqArg, "LANG", "Overrides the default language detection with LANG")
    , (MainClass, 'm', "main",    reqArg, "FILE", "Includes and selects FILE as the mainclass")
    , (Silent,    's', "silent",  noArg, [], "Silence all output returning and uses return codes instead."
                        ++ "\n  Return code -1: An error occured before or during submission to kattis"
                        ++ "\n  Return code  0: Successfully submitted and all tests passed"
                        ++ "\n  Return code  X: Submission failed at test X on kattis")
    , (Verbose,   'v', "verbose", noArg, [], "Enables verbose output, useful for debugging")
    , (Version,   'V', "version", noArg, [], "Displays version information")
    ]
