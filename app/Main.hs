{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings #-}
import Control.Concurrent
import Control.Monad.Writer.Lazy
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import Hattis.Error
import Hattis.Network
import Hattis.Text.Ini
import Hattis.Text.Make
import Hattis.Text.SourceFile
import Options.Applicative
import System.Console.ANSI
import System.Environment
import System.Exit
import System.IO

newtype Hattis a = Hattis {
        runHattis :: ExceptT HattisError (WriterT [String] IO) a
    } deriving (Monad, Applicative, Functor, MonadIO,
                MonadError HattisError, MonadWriter [String])

hattisver = "v1.1.0"
versionstr = "Hattis " ++ hattisver ++"\nCopyright (C) 2015-2016 Emil Gedda"

data Input = Input { probid :: String, files :: [String],
                     conf :: Maybe String, force :: Bool,
                     lang :: Maybe String, mainclass :: Maybe String,
                     silent :: Bool, noglyphs :: Bool, nocolor :: Bool,
                     make :: Bool}

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
                <> help ("Silence almost all output, do not print any intermediary information until "
                         ++ "the submission has been accepted or rejected by kattis or upon other client "
                         ++ "sided errors. This implies --force."))
        <*> switch
                (long "no-glyphs"
                <> help "Disable UTF-8 glyphs in output. Translate glyphs into ASCII characters instead.")
        <*> switch
                (long "no-color"
                <> help "Disable all coloring of the output, while keeping the amount of output.")
        <*> switch
                (long "make"
                <> hidden
                <> help ("Generate a Makefile and save it in the current directory. "
                        ++ "Additional flags provided alongside --make will be stored in the Makefile."))
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


genmake input = do
    let opts = [ boolopt  "--force" . force
               , boolopt  "--silent" . silent
               , boolopt  "--no-glyphs" . noglyphs
               , boolopt  "--no-color" . nocolor
               , maybeopt "--conf" . conf
               , maybeopt "--language" . lang
               , maybeopt "--main" . mainclass]
    let str = makefile hattisver
                       (probid input)
                       (map show $ files input) -- quote files
                       $ map ($ input) opts
    liftIO $ putStr str

run :: Input -> Hattis ()
run input =
    if (make input)
    then genmake input
    else do

    let println = unless (silent input) . liftIO . putStrLn
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
    println $ "Problem ID: " ++ probid input
    println $ "Files:      " ++ intercalate "\n            " (files input)

    unless (force input || silent input) $ do
            liftIO $ putStr "Submit? (y/n): " *> hFlush stdout
            line <- liftIO $ map toLower <$> getLine
            unless (line `isPrefixOf` "yes") $ throwError SubmissionDenied

    println "Logging into kattis..."
    cookies <- wrap $ login user token lurl
    println "Submitting solution..."
    id <- wrap $ submit cookies
                        surl
                        (probid input)
                        (files input)
                        (name language)
                        (mainclass input)
                        Nothing
    println $ "Submission ID: " ++ show id

    println "Refreshing authorization tokens..."
    newcookies <- wrap $ login user token lurl

    println "Fetching status..."
    let action =  wrap $ parsesubmission user token newcookies id
    let d = (unless (silent input) .) . display (nocolor input) (noglyphs input)
    prog <- action
    progress (silent input) prog action New d

    println "Submission accepted."

-- TODO: Abstract and clean the shit out of this, and implement a correct Show for SubmissionProgress
progress silent (Finished score str r) _ _ disp = unless silent
                                                . liftIO $ disp False r
                                                *> putStrLn ""
                                                *> tryprint score "Score"
                                                *> putStrLn ("CPU time: " ++ str)

progress silent (Failed s e h c r@(Running passed tot)) _ _ disp = do
    unless (silent)  . liftIO $ disp True r
    unless silent . liftIO $ putStrLn ""
    unless silent . liftIO . putStrLn . bool (++"!") extrainfo (tot /= 0) $ s

    unless silent $ tryprint e "Error info"
    unless silent $ tryprint h "Hints about testcase"
    unless silent $ tryprint c "Compiler output"
    throwError $ MiscError "Submission rejected."
    where
          extrainfo x = x ++ " on test case "
                          ++ show (min tot $ passed + 1)
                          ++ " of "
                          ++ show tot ++ "!"


progress silent p f prev disp = do
    unless silent $ liftIO (toStr p prev)
    next <- f
    liftIO $ threadDelay 750000
    progress silent next f p disp
    where
          toStr Compiling Compiling  = return ()
          toStr Compiling _          = putStrLn "Compiling..."
          toStr Waiting Waiting      = return ()
          toStr Waiting _            = putStrLn "Waiting..."
          toStr r@(Running _ _) _ = disp False r
          toStr _ _ = return ()


display nocolor noglyphs hasfailed (Running passed tot) = do
    putStr "\r[ "
    let pass = if noglyphs then 'P' else '✓'
    let fail = if noglyphs then 'X' else '✗'
    let unkw = '-'

    condapply nocolor (colorme Green) . putStr $ replicate (fromIntegral passed) pass

    when (passed /= tot) $
        if hasfailed
        then condapply nocolor (colorme Red) $ putChar fail
        else putChar unkw

    putStr $ replicate (fromIntegral (tot - passed - 1)) unkw
    putStr $ " | " ++ mbunknown (min tot $ passed + 1) ++ "/" ++ mbunknown tot ++ " ]"
    hFlush stdout
    where colorme c f = setSGR [SetColor Foreground Dull c] *> f *> setSGR [Reset]
          condapply False f = f
          condapply True _ = id
          mbunknown x | x <= 0 = "?"
          mbunknown x | otherwise = show x


wrap x = Hattis . ExceptT . WriterT $ do
    val <- x
    return (val,[])

tryprint (Just x) str = liftIO $ putStrLn (str ++ ": " ++ x)
tryprint _ _ = liftIO $ return ()
