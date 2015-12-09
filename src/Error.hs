{-# LANGUAGE DeriveDataTypeable #-}
module Error where
import Data.Typeable
import Control.Exception

data KattisError
    = SettingsNotFound
    | NoInternet
    | UploadFailed
    | NoUserSection
    | MalformedSettings
    | MiscError String
    | UnknownExtension String
    deriving (Typeable)

instance Show KattisError where
    show SettingsNotFound  = "Unable to locate kattisrc"
    show NoInternet        = "Unable to find an active internet connection"
    show UploadFailed      = "The upload was unable to complete"
    show MalformedSettings = "Unable to extract username and token, and submissionurl from kattisrc"
    show (UnknownExtension e) = "Unknown extension found, automatic language detection failed on file: " ++ e
    show (MiscError str)   = str

instance Exception KattisError
