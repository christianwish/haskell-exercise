module GroupValues (groupValues) where
import Data.List (sort, sortOn, groupBy)
import Data.Char (toLower)

headToTail :: String -> Int -> String
headToTail str i = drop i str ++ take i str

cloneStr str = take (length str) (cycle [a])
    where a = map toLower str

mutationList _ [] = []
mutationList i (x:xs) = [headToTail x i] ++ mutationList (i + 1) xs

getSharedId xs = head $ sort $ mutationList 0 xs

withSharedId str = (str, getSharedId $ cloneStr str)

groupValues xs = map (map (\x -> fst x)) b
    where b = groupBy (\x y -> snd x == snd y) a
          a = sortOn snd $ map withSharedId xs

