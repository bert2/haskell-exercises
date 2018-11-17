module PutJSON (putJValue, renderJValue) where

import Data.List (intercalate)
import SimpleJSON

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool b)   = show b
renderJValue JNull       = "null"
renderJValue (JObject o) = "{" ++ renderPairs o ++ "}"
    where renderPairs = renderListWith renderPair
renderJValue (JArray a)  = "[" ++ renderValues a ++ "]"
    where renderValues = renderListWith renderJValue

renderPair :: (String, JValue) -> String
renderPair (k, v) = show k ++ ": " ++ renderJValue v

renderListWith :: (a -> String) -> [a] -> String
renderListWith f = intercalate ", " . map f
