sndToLast :: [a] -> a
sndToLast []       = error "empty list"
sndToLast [_]      = error "not enough elements"
sndToLast [x,_]    = x
sndToLast (_:xs)   = sndToLast xs
