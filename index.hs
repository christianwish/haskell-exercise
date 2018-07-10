import Data.Char (toLower)

headToTail (h:str) = str ++ [h]
toLowerString = map toLower

sameChars :: Int -> String -> String -> Bool
sameChars i str1 str2 = if (str1 == str2)
    then True
    else if (i >= length str2 || length str1 /= length str2)
        then False
        else sameChars (i + 1) (headToTail str1) str2

areLowerSame s1 s2 = sameChars 0 lowerS1 lowerS2
    where
        lowerS1 = toLowerString s1
        lowerS2 = toLowerString s2

wrap x = x:[]

prepare str = map wrap $ words str

task = do
     let stringList = prepare "Tokyo London Rome Donlon Kyoto"
     print(stringList)
-- [["Test"],["test"],["ttt"]]
