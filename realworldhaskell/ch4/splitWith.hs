import Data.List

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p = filter (p . head) . groupBy (\x1 x2 -> p x1 && p x2)