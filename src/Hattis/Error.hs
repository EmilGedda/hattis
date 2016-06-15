{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hattis.Error (ErrorT) where

import Control.Monad.Except

newtype ErrorT a = ErrorT { runError :: ExceptT String IO a } 
                deriving (Monad, Functor, Applicative)
    -- ^^ Should we really use String here? Looks ugly.

unwrapError :: ErrorT a -> IO (Either String a)
unwrapError = runExceptT . runError

joinIO :: IO (ErrorT a) -> ErrorT a
joinIO = ErrorT . ExceptT. (unwrapError =<<) 
