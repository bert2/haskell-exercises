import Data.List (sortBy)
import Data.Tuple (swap)

type Point = (Double, Double)
type Vector = Point

data Direction = LeftTurn | Straight | RightTurn
    deriving (Eq, Show)

direction :: (Point, Point, Point) -> Direction
direction (p1, p2, p3)
    | z > 0     = LeftTurn
    | z < 0     = RightTurn
    | otherwise = Straight
    where z = cross (vec p1 p2) (vec p1 p3)

isLeftTurn :: (Point, Point, Point) -> Bool
isLeftTurn = (==) LeftTurn . direction

directions :: [Point] -> [Direction]
directions = map direction . win3

convexHull :: [Point] -> [Point]
convexHull ps =
    let s = minimumOn swap ps
        rest = filter (/=s) ps
        ps' = s : sortBy (polarOrderFrom s) rest
    in reverse $ tail $ foldl' addOuter [] $ close ps'
    where addOuter (p2:p1:ps'') p3 | isLeftTurn (p1,p2,p3) = p3:p2:p1:ps''
                                   | otherwise             = addOuter (p1:ps'') p3
          addOuter ps''         p  = p:ps''

-- less complicated, but also less efficient.
convexHull' :: [Point] -> [Point]
convexHull' ps =
    let s = minimumOn swap ps
        rest = filter (/=s) ps
        ps' = s : sortBy (polarOrderFrom s) rest
    in converge delInner ps'
    where delInner ps'' = s : [middle l | l <- win3 $ close ps'', isLeftTurn l]

vec :: Point -> Point -> Vector
vec (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

cross :: Vector -> Vector -> Double
cross (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

polarOrderFrom :: Point -> Point -> Point -> Ordering
polarOrderFrom o a b = let dir = direction (o, a, b)
                       in case dir of LeftTurn  -> LT
                                      Straight  -> EQ
                                      RightTurn -> GT

win3 :: [a] -> [(a,a,a)]
win3 (l:x:r:xs) = (l,x,r) : win3 (x:r:xs)
win3 _          = []

minimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
minimumOn f = foldr1 (\x z -> if f x < f z then x else z)

converge :: Eq a => (a -> a) -> a -> a
converge f x | x' == x   = x'
             | otherwise = converge f x'
             where x' = f x

middle :: (a, b, c) -> b
middle (_, x, _) = x

close :: [a] -> [a]
close []     = error "empty list"
close (x:xs) = (x:xs) ++ [x]
