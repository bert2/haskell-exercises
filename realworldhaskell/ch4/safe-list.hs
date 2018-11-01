-- safeHead and safeLast are stupid, but kinda interesting

safeHead :: [a] -> Maybe a
safeHead = foldr (const . Just) Nothing

safeLast :: [a] -> Maybe a
safeLast = foldl (const Just) Nothing

safeTail :: [a] -> Maybe [a]
safeTail = safe tail

safeInit :: [a] -> Maybe [a]
safeInit = safe init

safe f xs | null xs   = Nothing
          | otherwise = Just (f xs)