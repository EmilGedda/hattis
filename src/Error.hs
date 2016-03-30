{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Error where
import Control.Monad.Except
import Data.List

newtype KattisApp a = KattisApp {
                        runApp :: ExceptT KattisError IO a
                } deriving (Monad, Applicative, Functor)


wrapKattis = KattisApp . ExceptT
unwrapKattis = runExceptT . runApp

joinIO :: IO (KattisApp a) -> KattisApp a
joinIO = wrapKattis . join . return . (unwrapKattis =<<)
 

data KattisError
    = SettingsNotFound
    | NoInternet
    | UploadFailed
    | NoUserSection
    | MalformedSettings
    | MiscError String
    | UnknownExtension String
    | MultipleLanguages [String] 
    | NotAFile [String]
    | UnknownLanguage String
    | ErroneousSettings String String
    | ParseFail String

instance Show KattisError where
    show SettingsNotFound  = "Unable to locate kattisrc"
    show NoInternet        = "Unable to find an active internet connection"
    show UploadFailed      = "The upload was unable to complete"
    show MalformedSettings = "Unable to extract username and token, and submissionurl from kattisrc"
    show (UnknownExtension e)  = "Unknown extension found, automatic language detection failed on file: " ++ e
    show (MultipleLanguages x) = "Unable to automatically decide a language.\nPossible matches: " ++ intercalate ", " x
    show (MiscError str)    = str
    show (NotAFile files)   = "Provided arguments do not resolve to existing files: " ++ intercalate ", " files
    show (UnknownLanguage l) = "Provided language is not supported for language: " ++ l
    show (ErroneousSettings sec key) = "Unable to find key '" ++ key ++ "' in section '" ++ sec ++ "', or key/value is malformed. \n Make sure the kattisrcis correctly formatted."
    show (ParseFail str) = str


