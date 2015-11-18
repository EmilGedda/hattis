{-# LANGUAGE DeriveDataTypeable #-}
module Error where
import Data.Typeable
import Control.Exception

data KattisError
    = SettingsNotFound
    | NoInternet
    | UploadFailed
    | MiscError String
    deriving (Typeable)

instance Show KattisError where
    show SettingsNotFound = "Unable to locate kattisrc"
    show NoInternet       = "Unable to find an active internet connection"
    show UploadFailed     = "The upload was unable to complete"
    show (MiscError str)  = str

instance Exception KattisError
