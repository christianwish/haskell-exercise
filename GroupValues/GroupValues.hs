module GroupValues where

import Data.List (zipWith)
import Data.Char (toLower)

toLowerStr = map toLower

headToTail :: String -> Int -> String
headToTail str i = (drop i str) ++ (take i str)

imap :: (a -> Int -> b) -> [a] -> [b]
imap fn list = zipWith fn list [0..]

cloneList :: String -> [String]
cloneList str =  take len strs
    where
    len = length str
    strs = repeat str

mutationList :: String -> [String]
mutationList str = imap headToTail (cloneList str)

areSame :: String -> String -> Bool
areSame s1 s2 = s1' `elem` s2s
    where
    s1' = toLowerStr s1
    s2s = mutationList $ toLowerStr s2

areNotSame :: String -> String -> Bool
areNotSame s1 s2 = not $ areSame s1 s2

groupValues :: [String] -> [[String]]
groupValues (x:xs) = a:[] ++ (groupValues b)
    where
    a = x:(filter (areSame x) xs)
    b = filter (areNotSame x) xs
groupValues _ = []

