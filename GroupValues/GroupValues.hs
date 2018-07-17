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

cSort (_,a) (_,b) = compare a b
cGroup (_,a) (_,b) = a == b
s = sortBy cSort
firstOfMutationList x = sort $ head $ mutationList $ map toLower x
addToMutationList = map (\x -> (x, firstOfMutationList x))
g = groupBy cGroup
toResult = map (map (\(x,_) -> x))

groupValues = toResult . g . s . addToMutationList

