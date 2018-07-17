module GroupValuesCompact where

import Data.List (zipWith, sort, sortBy, groupBy)
import Data.Char (toLower)

headToTail str i = (drop i str) ++ (take i str)
imap fn list = zipWith fn list [0..]
mutationList str = imap headToTail (take (length str) (repeat str))
firstOfMutationList x = sort $ head $ mutationList $ map toLower x
addToMutationList = map (\x -> (x, firstOfMutationList x))
s = sortBy (\(_,x) (_,y) -> compare x y)
g = groupBy (\(_,a) (_,b) -> a == b)
cleanFlat = map (map (\(x,_) -> x))
groupValuesCompact = cleanFlat . g . s . addToMutationList
