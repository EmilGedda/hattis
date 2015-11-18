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
    deriving (Typeable)

instance Show KattisError where
    show SettingsNotFound  = "Unable to locate kattisrc"
    show NoInternet        = "Unable to find an active internet connection"
    show UploadFailed      = "The upload was unable to complete"
    show NoUserSection     = "Unable to find a [user] section in kattisrc"
    show MalformedSettings = "Unable to extract username and token from the [user] section in kattisrc"
    show (MiscError str)   = str

instance Exception KattisError
