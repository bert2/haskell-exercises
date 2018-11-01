import System.Environment (getArgs)
import Safe (headDef)
import Data.List (transpose)

interactWith f inFile outFile = do
    input <- readFile inFile
    writeFile outFile (f input)

main = mainWith transposeLines
    where mainWith f = do
              args <- getArgs
              case args of
                  [arg1, arg2] -> interactWith f arg1 arg2
                  _ -> putStrLn "error: exactly two arguments needed"

          firstWords = unlines . map firstWord . lines
                where firstWord = (headDef "") . words

          transposeLines = unlines . transpose . lines