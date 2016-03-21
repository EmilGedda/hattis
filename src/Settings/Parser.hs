module Parser where
import Lexer
import Data.Char
import Data.List
import System.Directory

type Schema = [Spec (String -> Bool)]
type Settings = [Spec String]

data Spec a 
    = Section String [Spec a]
    | KeyVal String a 

instance Eq (Spec x) where 
        (==) (Section a _) (Section b _) = a == b
        (==) (KeyVal a _)  (KeyVal b _)  = a == b

instance Ord (Spec x) where
        compare (Section a _) (Section b _) = compare a b
        compare (KeyVal  a _) (KeyVal  b _) = compare a b

instance Show (Spec x) where
        show (Section a b) = a ++ ": [" ++ intercalate "," (map show b) ++ "]"
        show (KeyVal a _)  = a

valid :: Schema
valid = let f fun = all (==True) . map fun in
        [Section "user" [
                KeyVal "username" (f isAlpha),      -- TODO: Research
                KeyVal "token"    (f isHexDigit)
            ],
         Section "kattis" [
                KeyVal "loginurl"      (f isAscii), -- TODO: Fix URL
                KeyVal "submissionurl" (f isAscii)
            ]
        ]

check (KeyVal a f) (KeyVal b c) = a == b && f c
check (Section a x) (Section b y) = a == b && all (==True) (zipWith check x y)
