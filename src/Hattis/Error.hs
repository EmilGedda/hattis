module Hattis.Error (HattisError(..), module Control.Monad.Except) where

import Control.Monad.Except
import Data.List

data HattisError
    = ErroneousSettings String String
    | LoginFailed Int String
    | MalformedSettings
    | MiscError String
    | MultipleLanguages [String] 
    | NoInternet
    | NotAFile [String]
    | ParseFail String
    | SettingsNotFound
    | TestCaseFailed Int String (Maybe String)
    | UnknownExtension String
    | UnknownLanguage String
    | SubmissionFailed Int
    | SubmissionDenied

instance Show HattisError where
    show (LoginFailed code msg) = "Unable to login! The server responded with: " ++ show code ++ " " ++ msg
    show SettingsNotFound  = "Unable to locate kattisrc"
    show NoInternet        = "Unable to find an active internet connection"
    show (SubmissionFailed code) = "The upload was unable to complete.\nKattis rejected the submission with http error code: " ++ show code
    show MalformedSettings = "Unable to extract username and token, and submissionurl from kattisrc"
    show (UnknownExtension e)  = "Unknown extension found, automatic language detection failed on file: " ++ e
    show (MultipleLanguages x) = "Unable to automatically decide a language.\nPossible matches: " ++ intercalate ", " x
    show (MiscError str)    = str
    show (NotAFile files)   = "Provided arguments do not resolve to existing files: " ++ intercalate ", " files
    show (UnknownLanguage l) = "Provided language is not supported for language: " ++ l
    show (ErroneousSettings sec key) = "Unable to find key '" ++ key ++ "' in section '" ++ sec ++ "', or key/value is malformed.\nMake sure the kattisrc is correctly formatted."
    show (ParseFail str) = "An error occured while parsing kattisrc:\n" ++  str
    show (TestCaseFailed num err desc) = "Submission failed at test case: " ++ show num ++ "\nError by kattis:\n" ++ err ++ maybe "" ("Test case description:\n"++) desc
    show SubmissionDenied = "Submission declined."
