{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Hattis.Network(login, submit) where
import Control.Arrow
import Control.Exception (try)
import Data.ByteString.Lazy hiding (putStrLn, map)
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Maybe
import Hattis.Error
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Conduit
import Network.HTTP.Simple
import System.FilePath
import qualified Data.Text as T

login 
    :: (MonadIO mio, MonadError HattisError merr) 
        => String 
        -> String 
        -> String 
        -> mio (merr CookieJar)
login user token url = liftIO $ do
        req' <- parseRequest url
        let request 
                = urlEncodedBody [ ("user", B.fromString user)
                                 , ("token", B.fromString token)
                                 , ("script", "true")]
                $ setRequestSecure True
                $ setRequestPort 443 -- necessary?
                $ req'
        mbresponse <- try $ httpLBS request
        return $ case mbresponse of
            Left err       -> throwError . MiscError $ show (err :: HttpException)
            Right response -> case getResponseStatusCode response of 
                                    200  -> return $ responseCookieJar response
                                    code -> throwError $ LoginFailed code "nomsg" -- TODO: fix msg

--login :: (MonadIO mio, MonadError HattisError merr) => ByteString -> ByteString -> String -> mio (merr CookieJar)
login' user token url = liftIO $ do --used for testing only
        req' <- parseRequest url
        let request 
                = urlEncodedBody [("user", user), ("token", token), ("script", "true")]
                $ setRequestSecure True
                $ setRequestPort 443 -- necessary?
                $ req'
        response <- httpLBS request
        putStrLn . show $ getResponseBody response
        return $ responseCookieJar response

--submit
--  :: (MonadIO mio, MonadError HattisError merr)
--     => CookieJar
--     -> String
--     -> String
--     -> [FilePath]
--     -> String
--     -> Maybe String
--     -> Maybe String
--     -> mio (merr String)
submit cookiejar url prob files lang main tag = liftIO $ do
        req' <- parseRequest url
        let reqheaders
                = setRequestSecure True
                $ setRequestPort 443
                $ req' { cookieJar = Just cookiejar }
        let parts = map (uncurry partBS) [ ("submit", "true")
                                       , ("submit_ctr", "2")
                                       , ("script", "true")
                                       , ("language", B.fromString lang)
                                       , ("mainclass", fromMaybe ""  $ B.fromString <$> main)
                                       , ("problem", B.fromString prob)
                                       , ("tag", fromMaybe "" $ B.fromString <$> tag)
                                       , ("type", "files")]
                                       ++ map (partFile "sub_file[]")  files
        boundary <- webkitBoundary
        request  <- formDataBodyWithBoundary boundary parts reqheaders
        mbresponse <- try $ httpLBS request
        return $ case mbresponse of
            Left err       -> throwError . MiscError $ show (err :: HttpException)
            Right response -> case getResponseStatusCode response of
                                    200  -> return . LB.toString $ getResponseBody response
                                    code -> throwError $ SubmissionFailed code -- TODO: fix msg

testsubmit user token loginurl submurl prob files lang = do
            cookies <- login' user token loginurl
            submit cookies submurl prob files lang Nothing Nothing
    
