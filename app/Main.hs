{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings #-}
import Control.Concurrent
import Control.Monad.Writer.Lazy
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import Hattis.Error
import Hattis.Network
import Hattis.Text.Ini
import Hattis.Text.SourceFile
import Options.Applicative
import System.Environment
import System.Exit
import System.IO

newtype Hattis a = Hattis {
        runHattis :: ExceptT HattisError (WriterT [String] IO) a
    } deriving (Monad, Applicative, Functor, MonadIO, 
                MonadError HattisError, MonadWriter [String])

hattisver = "v1.0.0"
versionstr = "hattis " ++ hattisver ++"\nCopyright (C) 2016 Emil Gedda"

data Verbosity = Silent | Normal
data Input = Input { probid :: String, files :: [String], 
                     conf :: Maybe String, force :: Bool,
                     lang :: Maybe String, mainclass :: Maybe String,
                     silent :: Bool } 

cmdopts :: Parser Input
cmdopts = Input
        <$> argument str 
                (metavar "PROBLEMID")
        <*> some 
            (argument str
                (metavar "FILES..."))
        <*> optional 
            (strOption
                (long "conf"
                <> short 'c'
                <> metavar "KATTISRC"
                <> help "Location of kattisrc file"))
        <*> switch
                (long "force"
                <> short 'f'
                <> help "Force submission without confirmation")
        <*> optional 
            (strOption
                (long "language"
                <> short 'l'
                <> metavar "LANGUAGE"
                <> help ("Override automatic language selection with LANGUAGE. "
                        ++ "Available languages are: " ++ intercalate ", " (map name langs))))
        <*> optional 
            (strOption
                (long "main"
                <> short 'm'
                <> metavar "MAINCLASS"
                <> help ("Override automatic main class detection when submitting Java solutions, "
                        ++ "MAINCLASS should be a file passed with FILES. "
                        ++ "This may cause issues at kattis whenever the language chosen is not Java.")))
        <*> switch 
                (long "silent"
                <> short 's'
                <> help ("Silence all output, instead return codes are used for communication."
                        ++ " Assuming 32 bit integer, a negative return code corresponds to a failure"
                        ++ " before submission to kattis. A positive return code (>0) corresponds to"
                        ++ " a test case on kattis the submission failed. A return code of 0 means"
                        ++ " the submission was submited succesfully and passed all test cases."))

main :: IO ()
main = execParser opts >>= maybe (putStrLn versionstr) (finalize <*> run)
    where veropts = flag' Nothing (long "version" 
                                    <> help "Display hattis version" 
                                    <> hidden) <|> (Just <$> cmdopts)
          opts = info (helper <*> veropts)
            (fullDesc
            <> progDesc "Submit a solution to a problem on kattis"
            <> header "Hattis - A command line interface to the online judge coding kattis")

finalize :: Input -> Hattis () -> IO a 
finalize i w = do
    ~(res, out) <- runWriterT . runExceptT . runHattis $ w  
    unless (silent i) (mapM_ putStrLn out)
    exit <- catcherr res
    exitWith exit

catcherr :: Either HattisError a -> IO ExitCode
catcherr (Left err) = do 
            print err
            return $ case err of
                TestCaseFailed num _ _ -> ExitFailure num 
                SubmissionDenied -> ExitSuccess
                _ -> ExitFailure 1
catcherr _ = return ExitSuccess

run :: Input -> Hattis ()
run input = do 
    let problem = probid input
    println "Loading settings..."
    settings <- wrap $ maybe (loadSettings []) loadSettings (conf input)

    user  <- getsetting Username settings
    token <- getsetting Token settings
    lurl  <- getsetting LoginUrl settings
    surl  <- getsetting SubmissionUrl settings

    println "Deciding language..."
    language <- wrap $ maybe (verifyfiles $ files input) (return . fromStr) (lang input)
    println $ "User:       " ++ user
    println $ "Language:   " ++ name language
    println $ "Problem ID: " ++ problem
    println $ "Files:      " ++ intercalate ", " (files input)

    unless (force input) $ do
            liftIO $ putStr "Submit? (y/n): " *> hFlush stdout
            line <- liftIO $ map toLower <$> getLine
            unless (line `isPrefixOf` "yes") $ throwError SubmissionDenied

    println "Logging into kattis..."
    cookies <- wrap $ login user token lurl
    println "Submitting solution..."
    id <- wrap $ submit cookies surl problem  (files input) (name language) Nothing Nothing
    println $ "Submission ID: " ++ show id

wrap x = Hattis . ExceptT . WriterT $ do
    val <- x
    return (val,[])

println = liftIO . putStrLn 
