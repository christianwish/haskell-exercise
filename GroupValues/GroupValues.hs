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

c (_,a) (_,b) = compare a b
c2 (_,a) (_,b) = a == b
s = sortBy c
addToMutationList = map (\x -> (x, sort $ head $ mutationList $ map toLower x))
g = groupBy c2
toResult = map (map (\(x,_) -> x))

groupValues = toResult . g . s . addToMutationList

