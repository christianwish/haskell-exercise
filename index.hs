import Data.Char (toLower)
-- Helper
headToTail (h:str) = str ++ [h]
toLowerStr = map toLower
wrap x = x:[]
prepare str = map wrap $ words str

sameChars :: Int -> String -> String -> Bool
sameChars i str1 str2 = if (str1 == str2)
    then True
    else if (i >= length str2 || length str1 /= length str2)
        then False
        else sameChars (i + 1) (headToTail str1) str2

areLowerSame s1 s2 = sameChars 0 (toLowerStr s1) (toLowerStr s2)
firstSame (s1:xs1) (s2:xs2) = areLowerSame s1 s2
groupItems ls1 ls2 = if (firstSame ls1 ls2) then [ls1 ++ ls2] else [ls1, ls2]

r acc (x:xs) = acc

task = do
    let stringList = prepare "Tokyo London Rome Donlon Kyoto"
        result = foldl r stringList []
    print(result)

-- [["Tokyo"],["London"],["Rome"],["Donlon"],["Kyoto"]]
