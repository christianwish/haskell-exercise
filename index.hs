import Data.Char (toLower)

toLowerStr = map toLower

headToTail :: String -> Int -> String
headToTail str i = (drop i str) ++ (take i str)

imap :: (a -> Int -> b) -> [a] -> [b]
imap fn list = zipWith fn list [0..]

cloneList :: String -> [String]
cloneList str =  length str `take` repeat str

mutationList :: String -> [String]
mutationList str = headToTail `imap` (cloneList str)

areSame :: String -> String -> Bool
areSame s1 s2 = toLowerStr s1 `elem` (mutationList $ toLowerStr s2)

innerSame :: [String] -> [String] -> Bool
innerSame (x:xs) (y:ys) = areSame x y

-- [["Tokyo"],["London"],["Rome"],["Donlon"],["Kyoto"]]
-- [["Tokyo"] ["Kyoto"]]
