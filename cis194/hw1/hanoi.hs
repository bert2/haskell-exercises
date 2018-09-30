type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 src dst _   = [(src, dst)]
hanoi n src dst tmp = allButLastToTmp ++ [(src, dst)] ++ allfromTmpToDst
    where allButLastToTmp = hanoi (n-1) src tmp dst
          allfromTmpToDst = hanoi (n-1) tmp dst src

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _           = []
hanoi4 1 src dst _ _       = [(src, dst)]
hanoi4 n src dst tmp1 tmp2 = fstHalveToTmp1 ++ sndHalveToTmp2 ++ [(src, dst)] ++ sndHalveToDst ++ fstHalveToDst
    where fstHalveToTmp1 = hanoi4 (n    `div` 2) src tmp1 tmp2 dst 
          sndHalveToTmp2 = hanoi4 (ndec `div` 2) src tmp2 dst tmp1
          sndHalveToDst  = hanoi4 (ndec `div` 2) tmp2 dst src tmp1
          fstHalveToDst  = hanoi4 (n    `div` 2) tmp1 dst tmp2 src
          ndec = n - 1