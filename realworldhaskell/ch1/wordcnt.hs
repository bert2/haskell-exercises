-- how to run:
-- > cat wordcnt.hs | stack runghc wordcnt
-- > 29
main = interact wordCount
    where wordCount input = show (length (words input)) ++ "\n"