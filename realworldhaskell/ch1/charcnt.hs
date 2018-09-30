-- how to run:
-- > cat charcnt.hs | stack runghc charcnt
-- > 149
main = interact charCount
    where charCount input = show (length input) ++ "\n"