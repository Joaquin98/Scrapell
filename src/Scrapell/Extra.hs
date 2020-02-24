module Scrapell.Extra (
 removeDups ,
 getDomain ,
 addDomain ,
 addHttps ,
 stringToDouble ,
 priceToDouble
) where
import Data.List

{-
  Funciones que pueden ser utiles.
-}

removeDups :: (Ord a) => [a] -> [a]
removeDups = map head . group . sort

getDomain :: String -> String
getDomain ss = getDomain' ss 0

getDomain' :: String -> Int -> String
getDomain' (s:ss) n
  | s == '/' && (n > 1) = []
  | s == '/' = s : (getDomain' ss (n+1))
  | otherwise = s : (getDomain' ss n)

addDomain :: String -> [String] -> [String]
addDomain url paths = let domain = getDomain url
                      in map ((++) domain) paths
addHttps :: [String] -> [String]
addHttps = map ((++) "https:")


stringToDouble :: String -> [(Double,String)]
stringToDouble = reads

argToUSA :: String -> String
argToUSA p = replace (replace p "." "" ) "," "."

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)

priceToDouble :: String -> Double
priceToDouble ('$':p) = let [(d,r)] = stringToDouble (argToUSA p) in d
priceToDouble p = let [(d,r)] = stringToDouble (argToUSA p) in d
