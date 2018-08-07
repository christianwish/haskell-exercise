import Data.Char (toLower)
import System.IO
import Control.Exception

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
countWords blacklist input = (a, b)
    where
        a = length restUniqeWords
        b = length restInputWords
        restUniqeWords = filter (`notElem` uniqBlacklist) uniqInput
        restInputWords = filter (`notElem` uniqBlacklist) allInputWords
        uniqInput = getUniqWords input
        allInputWords = words $ replaceNonWords input
        uniqBlacklist = getUniqWords blacklist

getFileContent :: String -> String -> IO String
getFileContent def filePath = readFile filePath `catch`
        \e -> const (return def) (e :: IOException)

main = do
    putStrLn "Eingabe:\n"
    userInput <- getLine -- Es blaut die Nacht, die Sternlein blinken
    fileContent <- getFileContent "" "./blacklist.txt"
    let result = countWords fileContent userInput
        s = (show $ snd result) ++ " Woerter, davon " ++ (show $ fst result) ++ " verschieden."
    putStrLn s
