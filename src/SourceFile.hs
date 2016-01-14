module SourceFile where
import Control.Arrow
import Data.Either
import Error
import System.FilePath

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
    exts Java       = [".java"]
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

possiblelangs :: String -> [Language]
possiblelangs ext = filter (matches ext) langs

getlangs :: Files -> Either KattisError [[Language]]
getlangs = sequence . map (fun . (id &&& (possiblelangs . takeExtension)))
                where fun (f, []) = Left (UnknownExtension f)
                      fun (_, l)  = Right l

decidelang :: Files -> Either KattisError [Language]
decidelang = (flip (filter . flip possible) langs <$>) . getlangs
                where possible x = foldr ((&&) . elem x) True

-- Clean up the structure, separate algorithms from the logic
-- Making the code more testable

