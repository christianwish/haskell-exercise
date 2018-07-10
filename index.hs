import Data.Char (toLower)

-- Helper
headToTail (h:str) = str ++ [h]
toLowerStr = map toLower
wrap x = x:[]
prepare str = map wrap $ words str

sameChars i str1 str2 = if (str1 == str2)
    then True
    else if (i >= length str2 || length str1 /= length str2)
        then False
        else sameChars (i + 1) (headToTail str1) str2

areLowerSame s1 s2 = sameChars 0 (toLowerStr s1) (toLowerStr s2)

task = do
    let stringList = prepare "Tokyo London Rome Donlon Kyoto"
    print(stringList)

-- [["Tokyo"],["London"],["Rome"],["Donlon"],["Kyoto"]]
