{-# LANGUAGE FlexibleContexts #-}
import Control.DeepSeq
import Hattis.Text.SourceFile
import Hattis.Text.Ini
import Hattis.Error
import Options.Applicative
import Data.Bifunctor
import Data.List
import Data.Maybe
import Hattis.Arguments
import System.Environment
import System.Exit

versionstr = "hattis v1.0.0\nCopyright (C) 2016 Emil Gedda"

data Verbosity = Silent | Normal
data Input = Input { id :: String, files :: [String], 
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
                        ++ "Available languages are: " ++ (intercalate ", " $ map name langs))))
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
main = execParser opts >>= maybe (putStrLn versionstr) run
    where veropts = flag' Nothing (long "version" 
                                    <> help "Display hattis version" 
                                    <> hidden) <|> (Just <$> cmdopts)
          opts = info (helper <*> veropts)
            (fullDesc
            <> progDesc "Submit a solution to a problem on kattis"
            <> header "Hattis - A command line interface to the online judge coding kattis")

handler x =  putStrLn "Error!" >> x

--run :: Input -> IO ()
run input = do 
    let tmp = loadSettings
    w <- maybe (tmp []) tmp (conf input) 
    --return () <* fmap putStrLn (getsetting Username w)

    --putStrLn "Here"

    return ()
