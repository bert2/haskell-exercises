module Prettify
    (
        Doc(), empty, char, text, double, line, softline,
        (<>), (</>), fold, hcat, fsep, punctuate,
        compact, pretty, fill
    ) where

import Prelude hiding ((<>))

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
    deriving (Eq, Show)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double n = text (show n)

line :: Doc
line = Line

softline :: Doc
softline = group line

(<>) :: Doc -> Doc -> Doc
x <> Empty = x
Empty <> y = y
x <> y     = x `Concat` y

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f Empty

hcat :: [Doc] -> Doc
hcat = fold (<>)

fsep :: [Doc] -> Doc
fsep = fold (</>)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

compact :: Doc -> String
compact x = transform [x]
    where transform []     = ""
          transform (d:ds) =
              case d of
                  Empty        -> transform ds
                  Char c       -> c : transform ds
                  Text s       -> s ++ transform ds
                  Line         -> '\n' : transform ds
                  x `Concat` y -> transform (x:y:ds)
                  _ `Union` y  -> transform (y:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best _   []     = ""
          best col (d:ds) =
              case d of
                  Empty        -> best col ds
                  Char c       -> c : best (col + 1) ds
                  Text s       -> s ++ best (col + length s) ds
                  Line         -> '\n' : best 0 ds
                  x `Concat` y -> best col (x:y:ds)
                  x `Union` y  -> nicest col (best col (x:ds))
                                             (best col (y:ds))
          nicest col x y | (width - least) `fits` x = x
                         | otherwise                = y
              where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

-- The exercises (http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html#id601782) are unclear to me.
-- I'll have to read and implement "A Pretty Printer" by Phillip Wadler first (https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).
