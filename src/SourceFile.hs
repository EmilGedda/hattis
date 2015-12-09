module SourceFile where
import Data.Either
import Error
import System.FilePath
import Control.Arrow
import Data.List (intercalate)

type Files = [String]

data Language --Todo: Sort by lexical order?
    = Java
    | Cpp
    | Go
    | C 
    | CSharp
    | JavaScript
    | ObjectiveC 
    | Haskell 
    | Prolog
    | PHP 
    | Ruby
    | Python2
    | Python3
    deriving (Show)

class FileExt a where
    exts :: a -> [String]
    name :: a -> String

instance FileExt Language where
    exts Java       = [".java"]
    exts Cpp        = [".cpp", ".cc", ".cxx", ".hpp", ".h"]
    exts C          = [".c", ".h"]
    exts Go         = [".go"]
    exts ObjectiveC = [".m", ".h"]
    exts Haskell    = [".hs"]
    exts Prolog     = [".pl", ".prolog"]
    exts PHP        = [".php"]
    exts Ruby       = [".rb"]
    exts Python3    = [".py"] --Todo: Solve python-ambiguity 
    exts Python2    = [".py"]
    exts CSharp     = [".cs"]
    exts JavaScript = [".js"]
    
    name Cpp        = "C++"
    name ObjectiveC = "Objective-C"
    name Python3    = "Python 3"
    name Python2    = "Python 2"
    name CSharp     = "C#"
    name x          = show x

langs :: [Language]
langs = [Java, Cpp, Go, C, CSharp, JavaScript, ObjectiveC, 
            Haskell, Prolog, PHP, Ruby, Python2, Python3]

matches :: String -> Language -> Bool
matches = (. exts) . elem

possiblelangs :: String -> [Language]
possiblelangs ext = filter (matches ext) langs

tmpname x = case (partitionEithers $ getlangs x) of
                ([], exts)  -> Right exts
                (l, _)      -> Left l

--validlangs :: Files -> Either [KattisError] Language
getlangs = map $ fun . (id &&& (possiblelangs . takeExtension))
                where fun (f, []) = Left (UnknownExtension f)
                      fun (_, l)  = Right l
