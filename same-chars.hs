headToTail str = [(drop 1 str) ++ (take 1 str)]

headToTailRec :: [String] -> Int -> [String]
headToTailRec acc i
    | (i == 0) = acc
    | otherwise = headToTailRec (acc ++ (headToTail $ last acc)) (i - 1)

getMutationArray :: String -> [String]
getMutationArray str = headToTailRec [str] (length str - 1)
