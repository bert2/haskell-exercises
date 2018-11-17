module Main where

import SimpleJSON
import PutJSON

main = let json = JObject [("foo", JNumber 1),
                           ("bar", JBool False),
                           ("fizz", JString "test"),
                           ("fuzz", JNumber 1.23),
                           ("yib", JNull),
                           ("yub", JArray [JNumber 0, JNumber 1, JNumber 2])]
       in putJValue json
