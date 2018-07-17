module GroupValues where

import Data.List (zipWith, sort, sortBy, groupBy)
import Data.Char (toLower)

headToTail :: String -> Int -> String
headToTail str i = (drop i str) ++ (take i str)

imap :: (a -> Int -> b) -> [a] -> [b]
imap fn list = zipWith fn list [0..]

cloneList :: String -> [String]
cloneList str =  take (length str) (repeat str)

mutationList :: String -> [String]
mutationList str = imap headToTail (cloneList str)

cSort :: (a, String) -> (b, String) -> Ordering
cSort (_,x) (_,y) = compare x y

s :: [(String, String)] -> [(String, String)]
s = sortBy cSort

cGroup :: (a, String) -> (b, String) -> Bool
cGroup (_,a) (_,b) = a == b

g :: [(String, String)] -> [[(String, String)]]
g = groupBy cGroup

firstOfMutationList :: String -> String
firstOfMutationList x = sort $ head $ mutationList $ map toLower x

addToMutationList = map (\x -> (x, firstOfMutationList x))

cleanFlat :: [[(String, String)]] -> [[String]]
cleanFlat = map (map (\(x,_) -> x))

groupValues ::[String] -> [[String]]
groupValues = cleanFlat . g . s . addToMutationList
