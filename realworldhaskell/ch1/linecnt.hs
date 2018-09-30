-- how to run:
-- > cat linecnt.hs | stack runghc linecnt
-- > 5
main = interact lineCount
    where lineCount input = show (length (lines input)) ++ "\n"