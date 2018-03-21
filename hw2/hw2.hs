import Data.List
import Text.Printf

-- 1 ---------------------------------------------------------------------------
saynumber :: Int -> Bool -> String
saynumber n feminine | n <  0    = unwords $ minus : say (-n) feminine
                     | n == 0    = zero
                     | otherwise = unwords $ say n feminine
    where
        say n feminine | n >= 10 ^ (21)         = ["WOW THATS BIG"]
                       |             n <    1 = []
                       |             n <   10 = if (feminine) then [femones !! (n - 1)] else [ones !! (n - 1)]
                       |  11 <= n && n <   20 = (twos !! (n `mod`  10 - 1) ++ twossfx) : []
                       |  10 <= n && n <  100 = (tens !! (n `div`  10 - 1)           ) : (say (n `mod`  10) feminine)
                       | 100 <= n && n < 1000 = (hdns !! (n `div` 100 - 1)           ) : (say (n `mod` 100) feminine)
                       | otherwise            = (reverse $ zip [0..] $ thnsplit n) >>= (say10 feminine)

        thnsplit 0 = []
        thnsplit n = (n `mod` 1000) : (thnsplit $ n `div` 1000)

        say10 feminine (0,    n) = say n feminine
     -- say10 _        (1,    1) = [agree 1 (snd $ thns !! 0)]  -- uncomment to replace 'one thousand' with just 'thousand'
        say10 _        (rank, n) = (say n feminine) ++ [agree n thn]
            where
                (feminine, thn) = thns !! (rank - 1)

        agree n xs = xs !! (rus_plural n)

        rus_plural n | n `mod` 10 == 1 && n `mod` 100 /= 11                                          = 0
                     | n `mod` 10 >= 2 && n `mod` 10 <= 4 && (n `mod` 100 < 10 || n `mod` 100 >= 20) = 1
                     | otherwise                                                                     = 2

        ones = ["один", "два", "три", "четыре", "пять", "шесть", "семь", "восемь", "девять"]
        femones = ["одна", "две", "три", "четыре", "пять", "шесть", "семь", "восемь", "девять"]
        twos = ["один", "две", "три", "четыр", "пят", "шес", "сем", "восем", "девят"]
        tens = ["десять", "двадцать", "тридцать", "сорок", "пятьдесят", "шестьдесят", "семьдесят", "восемьдесят", "девяносто"]
        hdns = ["сто", "двести", "триста", "четыреста", "пятьсот", "шестьсот", "семьсот", "восемьсот", "девятьсот"]
        thns = [
            (True,  ["тысяча", "тысячи", "тысяч"]),
            (False, ["миллион", "миллиона", "миллионов"]),
            (False, ["миллиард", "миллиарда", "миллиардов"]),
            (False, ["триллион", "триллиона", "триллионов"]),
            (False, ["квадриллион", "квадриллиона", "квадриллионов"]),
            (False, ["квинтиллион", "квинтиллиона", "квинтиллионов"])]
        twossfx = "надцать"
        zero = "ноль"
        minus = "минус"


-- 2 ---------------------------------------------------------------------------
hamming :: Eq a => [a] -> [a] -> Integer
hamming xs ys = sum $ zipWith eq xs ys
    where
        eq m n | m == n    = 0
               | otherwise = 1

-- 3 ---------------------------------------------------------------------------
gray :: Integral a => a -> [[Bool]]
gray 0 = [[]]
gray n = [False : x | x <- prev] ++ [True : x | x <- reverse prev]
  where prev = gray (n - 1)

-- 4 ---------------------------------------------------------------------------
paths :: Eq a => a -> a -> [(a, a)] -> [[a]] 
paths src dst edges | src == dst = [[dst]]
                    | otherwise  = [src:path | edge <- edges,
                                               (fst edge) == src,
                                               path <- furhter_paths edge]
    where
        furhter_paths edge = paths (snd edge) dst $ unwisited_edges edge
        unwisited_edges edge = [e | e <- edges, e /= edge]

-- 5 ---------------------------------------------------------------------------
components :: Eq a => [a] -> [(a, a)] -> [[a]]
components [] _                            = []
components nodes edges | unreachable == [] = [reachable]
                       | otherwise         = reachable : components unreachable edges
    where
        reachable = bfs nodes edges [head nodes]
        unreachable = nodes \\ reachable

        bfs []        _ _                                            = []
        bfs _         _ []                                           = []
        bfs nodes edges (current:stack) | not $ current `elem` nodes = bfs unwisited_nodes edges stack
                                        | otherwise                  = current : bfs unwisited_nodes edges (stack ++ neighbours)
            where
                neighbours = [x | (x, y) <- edges, y == current] ++ [x | (y, x) <- edges, y == current]
                unwisited_nodes = [x | x <- nodes, x /= current]


-- 6 ---------------------------------------------------------------------------
queens :: Int -> [[Int]]
queens 0 = [[]]
queens n = generate n
    where
        generate 0 = [[]]
        generate k = [q:qs | qs <- generate $ k - 1, q <- [1..n], check_rows q qs && check_diag q qs]
        check_rows q qs = not $ q `elem` qs
        check_diag q qs = not $ or $ zipWith (==) [1..] $ map (abs.(q-)) qs


-- Some printing ---------------------------------------------------------------

numbers = (map (0-) $ reverse xs) ++ [0] ++ xs
    where
        xs = [1, 2, 5, 10, 15, 20, 25, 50, 100, 101, 115, 125, 500, 1000, 4115, 10000, 5128255342891]

main = do
    putStr   $ id   $ "1:  "
    putStrLn $ id   $ intercalate "\n    " $ map (\n -> (printf "% 14d" n) ++ " -> " ++ saynumber n False) numbers

    putStr   $ id   $ "2:  [1, 2, 3] [1, 2, 3] -> "
    putStrLn $ show $ hamming [1, 2, 3] [1, 2, 3]
    putStr   $ id   $ "    [1, 5, 3] [1, 2, 3] -> "
    putStrLn $ show $ hamming [1, 5, 3] [1, 2, 3]

    putStr   $ id   $ "3:  "
    putStrLn $ show $ map (map fromEnum) $ gray 3

    putStr   $ id   $ "4:  1 4 [(1, 2), (2, 3), (1, 3), (3, 4), (4, 2), (5, 6)] -> "
    putStrLn $ show $ paths 1 4 [(1, 2), (2, 3), (1, 3), (3, 4), (4, 2), (5, 6)]

    putStr   $ id   $ "5:  [1, 2, 3, 4, 5, 6, 7, 8] [(1, 2), (2, 3), (5, 6), (7, 8)] -> "
    putStrLn $ show $ components [1, 2, 3, 4, 5, 6, 7, 8] [(1, 2), (2, 3), (5, 6), (7, 8)]

    putStr   $ id   $ "6:  "
    putStrLn $ id   $ intercalate "\n    " $ map show $ queens 8
