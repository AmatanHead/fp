-- 1 ---------------------------------------------------------------------------
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- 2 ---------------------------------------------------------------------------
nyas = [concat $ replicate n "nya" | n <- fibs]

-- 3 ---------------------------------------------------------------------------
fizz = map fizzify [1..]
    where
        fizzify n | n `mod` 15 == 0 = "FizzBuzz"
                  | n `mod` 3  == 0 = "Fizz"
                  | n `mod` 5  == 0 = "Buzz"
                  | otherwise       = show n

-- 4 ---------------------------------------------------------------------------
decsplit :: Integral t => t -> [t]
decsplit 0 = []
decsplit n = (n `mod` 10) : (decsplit $ n `div` 10)

-- 5 ---------------------------------------------------------------------------
luhn :: Integral t => [t] -> Bool
luhn xs = (sum $ map mut $ zip (reverse xs) [0..]) `mod` 10 == 0
    where
        mut (n, i) = if (i `mod` 2 == 0) then n else sum $ decsplit $ 2 * n

-- 6 ---------------------------------------------------------------------------
-- a)
interleave :: a -> [a] -> [[a]]
interleave n xs = [take i xs ++ [n] ++ drop i xs | i <- [0..length xs]]
-- b)
permutations :: [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) = permutations xs >>= interleave x

-- 7 ---------------------------------------------------------------------------
-- b)
thue_infinite = 'a' : 'b' : (tail thue_infinite >>= \x -> [x, not x])
    where
        not 'a' = 'b'
        not 'b' = 'a'
-- a)
thue :: Integral t => t -> String
thue n = take (2 ^ n) thue_infinite

-- Some printing ---------------------------------------------------------------

main = do
    putStr   $ id   $ "1:  "
    putStrLn $ show $ take 10 fibs

    putStr   $ id   $ "2:  "
    putStrLn $ show $ take 5 nyas

    putStr   $ id   $ "3:  "
    putStrLn $ show $ take 20 fizz

    putStr   $ id   $ "4:  "
    putStrLn $ show $ decsplit 123456

    putStr   $ id   $ "5:  4561 2612 1234 5464 (False) -> "
    putStrLn $ show $ luhn [4, 5, 6, 1, 2, 6, 1, 2, 1, 2, 3, 4, 5, 4, 6, 4];
    putStr   $ id   $ "    4561 2612 1234 5467 (True)  -> "
    putStrLn $ show $ luhn [4, 5, 6, 1, 2, 6, 1, 2, 1, 2, 3, 4, 5, 4, 6, 7];

    putStr   $ id   $ "6a: "
    putStrLn $ show $ interleave 5 [1,2,3,4]

    putStr   $ id   $ "6b: "
    putStrLn $ show $ permutations [1,2,3]

    putStr   $ id   $ "7a: "
    putStrLn $ show $ thue 6

    putStr   $ id   $ "7b: "
    putStrLn $ show $ take 64 thue_infinite
