module Shape where

data Shape = Circle {
                center :: (Double, Double),
                diameter :: Double }
           | Ellipse {
                center :: (Double, Double),
                width :: Double,
                height :: Double }
           | Square {
                topleft :: (Double, Double),
                width :: Double }
           | Foo {
                id :: Int }
    deriving Show
