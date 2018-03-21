{-# LANGUAGE RankNTypes #-}

-- 7 ---------------------------------------------------------------------------
applyToTuple :: (forall x. [x] -> Int) -> ([a], [b]) -> (Int, Int)
applyToTuple f (a, b) = (f a, f b)

-- Some printing ---------------------------------------------------------------

main = do
    putStr   $ id   $ "7:  "
    putStrLn $ show $ applyToTuple length ("hello", [1, 2, 3])

-- Я не знаю, требовалось ли общее решение в духе (1), но формально задание выполнено.
--
-- 1:
-- applyToTuple :: (forall x. x -> f x) -> (a, b) -> (f a, f b)
-- applyToTuple f (a, b) = (f a, f b)
