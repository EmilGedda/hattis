{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hattis.Error (HattisError(..), module Control.Monad.Except) where

import Control.Monad.Except
import Data.List

data HattisError
    = ErroneousSettings String String
    | MalformedSettings
    | MiscError String
    | MultipleLanguages [String] 
    | NoInternet
    | NotAFile [String]
    | ParseFail String
    | SettingsNotFound
    | UnknownExtension String
    | UnknownLanguage String
    | UploadFailed

instance Show HattisError where
    show SettingsNotFound  = "Unable to locate kattisrc"
    show NoInternet        = "Unable to find an active internet connection"
    show UploadFailed      = "The upload was unable to complete"
    show MalformedSettings = "Unable to extract username and token, and submissionurl from kattisrc"
    show (UnknownExtension e)  = "Unknown extension found, automatic language detection failed on file: " ++ e
    show (MultipleLanguages x) = "Unable to automatically decide a language.\nPossible matches: " ++ intercalate ", " x
    show (MiscError str)    = str
    show (NotAFile files)   = "Provided arguments do not resolve to existing files: " ++ intercalate ", " files
    show (UnknownLanguage l) = "Provided language is not supported for language: " ++ l
    show (ErroneousSettings sec key) = "Unable to find key '" ++ key ++ "' in section '" ++ sec ++ "', or key/value is malformed.\nMake sure the kattisrc is correctly formatted."
    show (ParseFail str) = "An error occured while parsing kattisrc:\n" ++  str
