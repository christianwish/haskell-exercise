import Data.Char (toLower)
import System.IO

-- Helper
toLowerString = map toLower
_NON_WORDS = ['.', ',', '?', '!', '"', '(', ')']
replaceChar x = if x `elem` _NON_WORDS then ' ' else x
replaceNonWords str = map replaceChar (toLowerString str)

uniqWords :: [String] -> String -> [String]
uniqWords [] w = w:[]
uniqWords acc w
    | w `elem` acc = acc
    | otherwise = w:[] ++ acc

getUniqWords :: String -> [String]
getUniqWords str = foldl uniqWords [] w
    where w = words clearedStr
          clearedStr = replaceNonWords str

countWords :: String -> String -> (Int, Int)
countWords blacklist input = (a, length restInputWords)
    where
        a = length restUniqeWords
        b = length restInputWords
        restUniqeWords = filter (`notElem` uniqBlacklist) uniqInput
        restInputWords = filter (`notElem` uniqBlacklist) allInputWords
        uniqInput = getUniqWords input
        allInputWords = words $ replaceNonWords input
        uniqBlacklist = getUniqWords blacklist

main = do
    putStrLn "Eingabe:\n"
    userInput <- getLine -- Es blaut die Nacht, die Sternlein blinken
    handle <- openFile "./blacklist.txt" ReadMode
    fileContent <- hGetContents handle
    let result = countWords fileContent userInput
        s = (show $ snd result) ++ " Woerter, davon " ++ (show $ fst result) ++ " verschieden."
    putStrLn s
