module SourceFile(Language, Files, FileExt, verifyfiles) where
import Control.Arrow
import Control.Monad
import Data.Either
import System.Directory
import Error
import System.FilePath
import qualified Data.List as L

type Files = [String]

data Language
    = C 
    | CSharp
    | Cpp
    | Go
    | Haskell 
    | Java
    | JavaScript
    | ObjectiveC 
    | PHP 
    | Prolog
    | Python2
    | Python3
    | Ruby
    deriving (Show, Eq)

class FileExt a where
    exts :: a -> [String]
    name :: a -> String

instance FileExt Language where
    exts C          = [".c", ".h"]
    exts CSharp     = [".cs"]
    exts Cpp        = [".cpp", ".cc", ".cxx", ".hpp", ".h"]
    exts Go         = [".go"]
    exts Haskell    = [".hs"]
    exts Java       = [".java"] --Todo: Find mainclass
    exts JavaScript = [".js"]
    exts ObjectiveC = [".m", ".h"]
    exts PHP        = [".php"]
    exts Prolog     = [".pl", ".prolog"]
    exts Python2    = [".py"]
    exts Python3    = [".py"] --Todo: Solve python-ambiguity 
    exts Ruby       = [".rb"]
    
    name CSharp     = "C#"
    name Cpp        = "C++"
    name ObjectiveC = "Objective-C"
    name Python2    = "Python 2"
    name Python3    = "Python 3"
    name x          = show x

langs :: [Language]
langs = [C, CSharp, Cpp, Go, Haskell, Java, JavaScript, 
        ObjectiveC, PHP, Prolog, Python2, Python3, Ruby] 

matches :: String -> Language -> Bool
matches = (. exts) . elem

allfiles :: Files -> KattisApp Files
allfiles x = wrapKattis $ do 
        nonexisting <- filter ((False==) . snd) . zip x <$> mapM doesFileExist x
        return $ case nonexisting of
                [] -> Right x
                l  -> Left . NotAFile $ map fst l

possiblelangs :: String -> [Language]
possiblelangs ext = filter (matches ext) langs

getlangs :: Files -> Either KattisError [[Language]]
getlangs = mapM (fun . (id &&& (possiblelangs . takeExtension)))
        where fun (f, []) = Left (UnknownExtension f)
              fun (_, l)  = Right l

decidelang :: Files -> Either KattisError Language
decidelang x = mul . (flip filter langs . flip possible) . zip x =<< getlangs x
        where possible x = foldr ((&&) . elem x . snd) True
              mul [a] = Right a
              mul  a  = Left . MultipleLanguages . map name $ a

verifyfiles :: Files -> KattisApp Language
verifyfiles = (wrapKattis . return . decidelang =<<) . allfiles
