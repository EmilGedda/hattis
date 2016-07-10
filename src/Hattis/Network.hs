{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Hattis.Network where
import Control.Arrow
import Control.Exception (try)
import Data.ByteString hiding (putStrLn, map)
import Data.Maybe
import Hattis.Error
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Conduit
import Network.HTTP.Simple
import System.FilePath
import qualified Data.Text as T

login :: (MonadIO mio, MonadError HattisError merr) => ByteString -> ByteString -> String -> mio (merr CookieJar)
login user token url = liftIO $ do
        req' <- parseRequest url
        let request 
                = setRequestQueryString [ ("user", Just user)
                                        , ("token", Just token)
                                        , ("script", Just "true")]
                $ setRequestSecure True
                $ setRequestPort 443 -- necessary?
                $ req'
        mbresponse <- try $ httpLBS request
        return $ case mbresponse of
            Left err       -> throwError . MiscError $ show (err :: HttpException)
            Right response -> case getResponseStatusCode response of
                                    200  -> return $ responseCookieJar response
                                    code -> throwError $ LoginFailed code "nomsg"

--login :: (MonadIO mio, MonadError HattisError merr) => ByteString -> ByteString -> String -> mio (merr CookieJar)
login' user token url = liftIO $ do
        req' <- parseRequest url
        let request 
                = setRequestQueryString [("user", Just user), ("token", Just token)]
                $ setRequestSecure True
                $ setRequestPort 443 -- necessary?
                $ req'
        response <- httpLBS request
        putStrLn . show $ responseHeaders response
        return $ responseCookieJar response

submit
  :: MonadIO m =>
     CookieJar
     -> String
     -> ByteString
     -> [FilePath]
     -> ByteString
     -> Maybe ByteString
     -> Maybe ByteString
     -> m ()
submit cookiejar url prob files lang main tag = liftIO $ do
        req' <- parseRequest url
        let reqheaders
                = setRequestQueryString [ ("submit", Just "true")
                                        , ("submit_ctr", Just "2")
                                        , ("script", Just "true")
                                        , ("language", Just lang)
                                        , ("mainclass", main)
                                        , ("problem", Just prob)
                                        , ("tag", tag)
                                        , ("type", Just "files")
                                        , ("sub_code", Nothing)]
                $ setRequestSecure True
                $ setRequestPort 443
                $ req' { cookieJar = Just cookiejar }
        let parts = map (partFile "sub_file[]")  files
        boundary <- webkitBoundary
        request <- formDataBodyWithBoundary boundary parts reqheaders
        let tmp = setRequestSecure True $ request {cookieJar = Just cookiejar }
        response <- httpLBS tmp
        putStrLn . show $ response

testsubmit user token loginurl submurl prob files lang = do
            cookies <- login' user token loginurl
            submit cookies submurl prob files lang Nothing Nothing
    
