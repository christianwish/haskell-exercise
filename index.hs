import Data.Char (toLower)

headToTail (h:str) = str ++ [h]
toLowerString = map toLower

sameChars :: Int -> String -> String -> Bool
sameChars index str1 str2 = if (str1 == str2)
    then True
    else
        if (index >= length str2 || length str1 /= length str2)
            then False
            else sameChars (index + 1) (headToTail str1) str2

areSame str1 str2 = sameChars 0 lowerStr1 lowerStr2
    where
        lowerStr1 = toLowerString str1
        lowerStr2 = toLowerString str2

-- c acc v =

-- doIt str = (foldl c) $ words str
