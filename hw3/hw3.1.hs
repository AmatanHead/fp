-- 3.1: Nim game

import Control.Monad.Except
import Data.Bits
import Data.Char
import System.IO
import Text.Read


-- Types -----------------------------------------------------------------------

-- Represents a single heap of objects.
type Heap = Word

-- An actual field consists of three heaps, A, B and C.
data Field = Field Heap Heap Heap deriving (Show)

-- Heaps are enumerated via theirs heap ids.
data HeapId = A | B | C deriving (Read, Show, Enum, Eq, Ord)

-- Player's move represented by heap id from which objects removed and
-- number of removed objects.
data Move = Move HeapId Word deriving (Eq)

instance Read Move where
  readsPrec _ (x:xs) | x `elem` "Aa" && hasValidDigit = [(Move A value, rest)]
                     | x `elem` "Bb" && hasValidDigit = [(Move B value, rest)]
                     | x `elem` "Cc" && hasValidDigit = [(Move C value, rest)]
                     | otherwise                      = []
    where
      tail = dropWhile isSpace xs
      hasValidDigit = hasDigit tail
      (_, value, rest) = readDigit tail

      hasDigit [] = False
      hasDigit (x:xs) = isDigit x

      readDigit [] = (0, 0, [])
      readDigit (x:xs) | isDigit x = (\(digit, v, rest) -> (digit + 1, v + (fromIntegral $ digitToInt x) * 10 ^ digit, rest)) (readDigit xs)
                       | otherwise = (0, 0, x:xs)

  readsPrec _ _ = []

instance Show Move where
  show (Move heap size) = (show heap) ++ " " ++ (show size)

-- Who's move it is.
data CurrentPlayer = Player | AI deriving (Show, Enum, Eq)


-- State modifications ---------------------------------------------------------

-- Checks whether the given move can be applied to the given field.
checkMove :: Field -> Move -> Bool
checkMove (Field _ _ _) (Move _ 0) = False
checkMove (Field a _ _) (Move A n) = a >= n
checkMove (Field _ b _) (Move B n) = b >= n
checkMove (Field _ _ c) (Move C n) = c >= n

-- Applies the move to the given field, rendering a new field.
applyMove :: (Monad m) => Field -> Move -> m Field
applyMove f m | checkMove f m = return $ applyMove' f m
              | otherwise     = fail "cannot apply this move to the given field"
  where
    applyMove' (Field a b c) (Move A n) = Field (a - n)  b       c
    applyMove' (Field a b c) (Move B n) = Field a       (b - n)  c
    applyMove' (Field a b c) (Move C n) = Field a        b      (c - n)


-- Winner strategy -------------------------------------------------------------

isGameOver :: Field -> Bool
isGameOver (Field 0 0 0) = True
isGameOver _             = False

-- Checks whether there is a winner strategy for the given field.
hasWinnerStrategy :: Field -> Bool
hasWinnerStrategy (Field a b c) = a `xor` b `xor` c > 0

-- Makes winner strategy or raises an error if there is none.
makeWinnerStrategy :: (Monad m) => Field -> m Move
makeWinnerStrategy f | hasWinnerStrategy f = return $ makeWinnerStrategy' f
                     | otherwise           = fail "there is no winner strategy"
  where
    makeWinnerStrategy' (Field a b c) = move
      where
        nimSum  = a `xor` b `xor` c
        nimBit  = (bit $ finiteBitSize nimSum - 1 - (countLeadingZeros nimSum)) :: Word
        move    = if      a .&. nimBit > 0 then Move A (a - (nimSum `xor` a))
                  else if b .&. nimBit > 0 then Move B (b - (nimSum `xor` b))
                  else                          Move C (c - (nimSum `xor` c))

-- Makes winner strategy if there is one,
-- makes any strategy if there is no winner strategy.
-- Any strategy is to remove one object from the biggest heap.
-- The idea is to maximize number of turns hoping that player make a mistake.
makeAnyStrategy :: (Monad m) => Field -> m Move
makeAnyStrategy f | hasWinnerStrategy f = makeWinnerStrategy f
                  | isGameOver f        = fail "field is empty"
                  | otherwise           = return $ makeAnyStrategy' f
  where
    makeAnyStrategy' (Field a b c) = Move (snd $ maximum [(a, A), (b, B), (c, C)]) 1


-- IO --------------------------------------------------------------------------

-- Try reading variable of type `a` until user supplies a valid input.
-- An additional check may be passed in form of a predicate which returns
-- either a value unchanged or an error message.
readUntilSuccessPredicate :: Read a => String -> (a -> Either String a) -> IO a
readUntilSuccessPredicate prompt predicate = do
  putStr prompt
  hFlush stdout

  line <- getLine

  case check $ readEither line of
    Right x -> return x
    Left  e -> onError e
  where
    check (Right x) = predicate x
    check (Left  e) = Left e
    onError e = do { putStrLn $ "Error: " ++ e; readUntilSuccessPredicate prompt predicate; }

-- Try reading variable of type `a` until user supplies a valid input.
readUntilSuccess :: Read a => String -> IO a
readUntilSuccess prompt = readUntilSuccessPredicate prompt Right

-- Show field state: before and after.
showFieldDiff :: Field -> Field -> IO ()
showFieldDiff (Field a b c) (Field a2 b2 c2) = do
  putStrLn $ ""
  putStrLn $ "  │"
  putStrLn $ "A │" ++ (row a2 " ◼") ++ (row (a - a2) " ◽") ++ rem (a - a2)
  putStrLn $ "B │" ++ (row b2 " ◼") ++ (row (b - b2) " ◽") ++ rem (b - b2)
  putStrLn $ "C │" ++ (row c2 " ◼") ++ (row (c - c2) " ◽") ++ rem (c - c2)
  putStrLn $ "  │"
  putStrLn $ ""
  where
    row x c = concat $ replicate (fromIntegral x) c
    rem 0 = ""
    rem 1 = "   (removed 1 object)"
    rem n = "   (removed " ++ (show n) ++ " objects)"


-- Actual game -----------------------------------------------------------------

aiMove :: Field -> IO Field
aiMove field = do {
  putStrLn $ concat $ replicate 70 "-";
  
  move <- makeAnyStrategy field;
  
  putStrLn $ "My move is " ++ (show move);
  
  newField <- applyMove field move;

  putStrLn $ concat $ replicate 70 "-";
  showFieldDiff field newField;

  return newField;
}

playerMove :: Field -> IO Field
playerMove field = do {
  putStrLn $ concat $ replicate 70 "-";

  move <- readUntilSuccessPredicate "Your move: " (movePredicate field) :: IO Move;

  newField <- applyMove field move;

  putStrLn $ concat $ replicate 70 "-";
  showFieldDiff field newField;

  return newField;
}
  where
    movePredicate field move | checkMove field move = Right move
                             | n == 0               = Left "nope!"
                             | otherwise            = Left "this is not a valid move"
      where
        Move _ n = move

game :: Field -> CurrentPlayer -> IO ()
game field Player | isGameOver field = do { putStrLn "I win! Ha! Bye, Human, see you!"; }
                  | otherwise        = do { newField <- playerMove field; game newField AI; }
game field AI     | isGameOver field = do { putStrLn "Looks like you win. Cool, see you!"; }
                  | otherwise        = do { newField <- aiMove field; game newField Player; }

main = do {
  putStrLn "Welcome to the Nim game.";
  putStrLn "";
  putStrLn "Rules (shortly). There are three heaps of objects (A, B, C).";
  putStrLn "Players take turns removing objects from heaps.";
  putStrLn "On each turn, a player choose a heap and remove";
  putStrLn "any (non-zero) number of objects from that heap.";
  putStrLn "Whoever removed the last object wins.";
  putStrLn "";
  putStrLn "Enter three numbers - heap sizes.";

  a <- readUntilSuccessPredicate "Heap A: " heapSizePredicate :: IO Heap;
  b <- readUntilSuccessPredicate "Heap B: " heapSizePredicate :: IO Heap;
  c <- readUntilSuccessPredicate "Heap C: " heapSizePredicate :: IO Heap;

  if (a == 0 && b == 0 && c == 0)
  then putStrLn $ "WHOA You win! Very clever of you!"
  else do {
    field <- return (Field a b c);

    putStrLn $ concat $ replicate 70 "-";

    if hasWinnerStrategy field
    then putStrLn $ "Let's go than! So the initial field is <" ++ (show field) ++ ">:"
    else putStrLn $ "Well, I feel like everything is falling out of my hands today...\nAnyway, starting position is <" ++ (show field) ++ ">:";

    showFieldDiff field field;

    game field AI;
  }
} `catchError` \e -> do {
  putStrLn $ id   $ "An error happened! Retreat! Retreat!..";
  putStrLn $ show $ e;
}
  where
    heapSizePredicate heap | heap <= 70 = Right heap
                           | otherwise  = Left "heap size should be between 0 and 20"
