module Utils (
    contains,
    remove,
    findUnitaryList,
    justAToA,
    toIo,
    findPositiveVar,
    set
) where

    import System.IO.Unsafe

    contains :: Eq a => a -> [a] -> Bool
    contains _ [] = False
    contains e (x:xs)
        |(e == x)  = True
        |otherwise = contains e xs 

    remove :: Eq a => a -> [a] -> [a]
    remove _ [] = []
    remove e (x:xs)
        | (e == x) = remove e xs
        | otherwise = x:(remove e xs)

    findUnitaryList :: [[Int]] -> Maybe Int
    findUnitaryList []     = Nothing
    findUnitaryList (x:xs)
        |(length x == 1) = Just (head x)
        |otherwise = findUnitaryList xs

    justAToA :: Maybe a -> a
    justAToA (Just a) = a

    toIo :: a -> IO a
    toIo a = do
        return a

    fromIo :: IO a -> a
    fromIo a = unsafePerformIO a

    emptyClause :: [[a]] -> Bool
    emptyClause [] = False
    emptyClause (x:xs)
        |(length x) == 0 = True
        |otherwise = emptyClause xs


    positivesVars :: [Int] -> Maybe Int
    positivesVars [] = Nothing
    positivesVars (x:xs)
        | (x > 0) = Just x
        | otherwise = positivesVars xs

    findPositiveVar :: [[Int]] -> Maybe Int
    findPositiveVar [] = Nothing
    findPositiveVar (x:xs)
        | (positivesVars x) /= Nothing = positivesVars x
        | otherwise = findPositiveVar xs


    set :: a -> [a] -> Int -> [a]
    set e xs i = (take i xs) ++ [e] ++ (drop (i+1) xs)

