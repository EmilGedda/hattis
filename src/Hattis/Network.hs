{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Hattis.Network(login, submit, parsesubmission, SubmissionProgress(..)) where
import Control.Arrow
import Control.Exception (try)
import Data.ByteString.Lazy hiding (putStrLn, map, dropWhile, drop, 
                                    takeWhile, length, span, take, any)
import Data.Char
import Data.List
import Data.Maybe
import Hattis.Error
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Text.XML.HXT.Core
import qualified Control.Monad as M
import qualified Data.ByteString.Lazy.UTF8 as LB
import qualified Data.ByteString.UTF8 as B

data SubmissionProgress 
            = Compiling
            | Waiting
            | New
            | Finished { time :: String, progr :: SubmissionProgress } 
            | Failed   { status:: String,
                         errinfo :: Maybe String, 
                         hints :: Maybe String, 
                         compilerinfo :: Maybe String,
                         progr :: SubmissionProgress }
            | Running  { currentcase :: Integer, 
                         totalcases :: Integer }
            deriving Show

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

submit
  :: (MonadIO mio, MonadError HattisError merr)
     => CookieJar
     -> String
     -> String
     -> [FilePath]
     -> String
     -> Maybe String
     -> Maybe String
     -> mio (merr Integer)
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
                                    200  -> return . filtr . LB.toString $ getResponseBody response
                                    code -> throwError $ SubmissionFailed code -- TODO: fix msg
        where filtr = read . takeWhile isDigit . dropWhile (not . isDigit)

parsesubmission 
    :: (MonadIO mio, MonadError HattisError merr)
        => String
        -> String
        -> CookieJar
        -> Integer
        -> mio (merr SubmissionProgress)
parsesubmission user token cookies id = liftIO $ do
        req' <- parseRequest $ "https://kth.kattis.com/submissions/" ++ show id 
        let request
                = setRequestSecure True
                $ setRequestPort 443
                $ req' { cookieJar = Just cookies }
        mbresponse <- try $ httpLBS request
        case mbresponse of
            Left err -> return . throwError . MiscError $ show (err :: HttpException)
            Right response -> case getResponseStatusCode response of
                                    200  -> return <$> (parseHTML . LB.toString $ getResponseBody response)
                                    code -> return . throwError . MiscError $ "Error! Submission page returned: " ++ show code
        
parseHTML :: MonadIO m => String -> m SubmissionProgress
parseHTML html = liftIO $ do
            let doc = readString [withParseHTML yes, withWarnings no] html 
            status <- parseStatus doc
            parse doc status
            where parse d x | x == "Accepted"  = parseFinished d <*> parseRunning d
                            | x == "Running"   = parseRunning  d
                            | x == "Waiting"   = return Waiting
                            | x == "Compiling" = return Compiling
                            | x == "New"       = return New
                            | otherwise        = parseFailed d x <*> parseRunning d

parseFinished :: MonadIO m => IOStateArrow () XmlTree XmlTree -> m (SubmissionProgress -> SubmissionProgress)
parseFinished doc = liftIO $ do
            let runtime = hasName "td" >>> hasAttrValue "class" (isInfixOf "runtime") 
            txt <- runX $ doc >>> deep runtime //> getText
            return . Finished . fromMaybe "Unknown time" 
                $ flip (++) "s" . takeWhile (isDigit `or` isPunctuation) <$> listToMaybe txt
                where f `or` g = (||) <$> f <*> g

parseRunning :: MonadIO m => IOStateArrow () XmlTree XmlTree -> m SubmissionProgress 
parseRunning doc = liftIO $ do
            let testcases =  hasName "div" >>> hasAttrValue "class" (=="testcases")
            uncurry Running . sumap (fromIntegral . length) . span (=="accepted")
                        <$> runX (doc >>> deep testcases /> getAttrValue "class")
            where sumap f = sec (+) . second f . first f
                  sec f (a,b) = (a, f a b)

parseFailed :: MonadIO m => IOStateArrow () XmlTree XmlTree -> String -> m (SubmissionProgress -> SubmissionProgress)
parseFailed doc status = liftIO $ do
            errinf  <- fetch "Error information"
            hints   <- fetch "Hints about test file"
            compinf <- fetch "Compiler output"
            return $ Failed status errinf hints compinf 
            where fetch txt = extract <$> runX (doc >>> deep (extrainfo txt) /> getChildren >>> getText)
                  extrainfo txt = isElem >>> hasName "div" </ (hasName "h3" /> hasText (==txt))
                  extract x = replace "\\n" "\n" . replace "\\t" "\t" <$> (listToMaybe $ drop 1 x)

parseStatus :: MonadIO m => IOStateArrow () XmlTree XmlTree -> m String
parseStatus doc = liftIO $ safe <$> runX (doc >>> deep stat //> getText)
            where stat = hasName "td" >>> hasAttrValue "class" (isInfixOf "status")
                  safe = fromMaybe "Unknown status" . listToMaybe

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ str = str
replace old new s@(x:xs) | take (length old) s == old 
                            = new ++ replace old new (drop (length old) s)
                         | otherwise = x:replace old new xs
replace _ _ _ = []
