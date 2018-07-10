headToTail str = [(drop 1 str) ++ (take 1 str)]

headToTailRec :: [String] -> Int -> [String]
headToTailRec acc i = if (i == 0)
    then acc
    else headToTailRec (acc ++ (headToTail $ last acc)) (i - 1)

getMutationArray :: String -> [String]
getMutationArray str = headToTailRec [str] (length str - 1)
