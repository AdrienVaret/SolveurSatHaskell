module Utils (
    contains,
    remove,
    findUnitaryList,
    justAToA,
    toIo
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