import Data.Char (toLower)
import System.IO

toLowerString = map toLower
_NON_WORDS = ['.', ',', '?', '!', '"', '(', ')']

replaceNonWords = map (\x -> if x `elem` _NON_WORDS then ' ' else x)

uniqWords :: [String] -> String -> [String]
uniqWords [] w = w:[]
uniqWords acc w
    | w `elem` acc = acc
    | otherwise = w:[] ++ acc

getWords :: String -> [String]
getWords str = foldl uniqWords [] w
    where w = map toLowerString (words clearedStr)
          clearedStr = replaceNonWords str

countWords :: String -> String -> Int
countWords blacklist input = length restWords
    where
        restWords = filter (\x -> x `notElem` uniqBlacklist) uniqInput
        uniqInput = getWords input
        uniqBlacklist = getWords blacklist

main = do
    putStrLn "Eingabe:\n"
    userInput <- getLine
    handle <- openFile "./blacklist.txt" ReadMode
    fileContent <- hGetContents handle
    print $ countWords fileContent userInput
