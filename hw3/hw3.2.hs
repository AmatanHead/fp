-- 3.2: Regexp matcher

import Control.Monad.Except
import System.IO
import Text.Read

import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Map.Strict ((!?))

import Data.Maybe (fromMaybe)


-- Helpers ---------------------------------------------------------------------

Just x  !?? y = x !? y
Nothing !?? y = Nothing


-- Types -----------------------------------------------------------------------

-- Regexp definition.
data RegExp = C Char
            | RegExp :|: RegExp  -- a or b
            | RegExp :-: RegExp  -- a than b
            | Eps
            | Ite RegExp
  deriving (Read, Show)

-- A single NDFA state.
data State = State Int deriving (Eq, Ord)

instance Show State where
  show (State s) = "S" ++ show s

-- A link between NDFA states (may be either char link or eps link).
data Link = CLink Char | ELink deriving (Show, Eq, Ord)

-- The NDFA itself
data NDFA = NDFA {
  initialState :: State,
  terminalStates :: Set State,
  allStates :: Set State,
  innerLinks :: Map State (Map Link (Set State))
}

instance Show NDFA where
  show ndfa = iss ++ tss ++ ass ++ sls
    where
      iss = "Initial state:  " ++ (show $ initialState ndfa) ++ "\n"
      tss = "terminalStates: " ++ (show $ Set.toList $ terminalStates ndfa) ++ "\n"
      ass = "allStates:      " ++ (show $ Set.toList $ allStates ndfa) ++ "\n"
      sls = showLinks $ Map.toList $ innerLinks ndfa
      showLinks [] = ""
      showLinks (x:xs) = (showLinkEnds (fst x) (Map.toList $ snd x)) ++ showLinks xs
      showLinkEnds _ [] = ""
      showLinkEnds s ((CLink c, states):xs) = (show s) ++ arrow ++ to ++ rest
        where
          arrow = " --" ++ [c] ++ "--> "
          to = (show $ Set.toList states)
          rest = "\n" ++ showLinkEnds s xs
      showLinkEnds s ((ELink,   states):xs) = (show s) ++ arrow ++ to ++ rest
        where
          arrow = " -----> "
          to = (show $ Set.toList states)
          rest = "\n" ++ showLinkEnds s xs


-- Library ---------------------------------------------------------------------

-- Compile regexp int an NDFA. This is rather naive compiler which yields
-- NDFAs with lots of unnecessary intermediate states and eps-links.
compile :: RegExp -> NDFA
compile r = transitiveClojure $ fst $ compileInner r [0..]
  where
    -- Merge two link maps.
    uniteLinks = Map.unionWith (Map.unionWith Set.union)

    -- Internal compilation procedure.
    -- Given a regexp definition and a state id generator,
    -- renders a NDFA and a new state id generator.
    compileInner :: RegExp -> [Int] -> (NDFA, [Int])
    -- Letter regext is a simple NDFA with two states and a single char link between them.
    --
    -- ──> () ──c──> () ──>
    --
    compileInner (C c) (s1:s2:ss) = (ndfa, ss)
      where
        iState = State s1
        tStates = Set.fromList [State s2]
        aStates = Set.fromList [State s1, State s2]
        links = Map.fromList [(iState, Map.fromList [(CLink c, tStates)])]
        
        ndfa  = NDFA iState tStates aStates links
    -- Conjunction NDFA is formed of two sub-NDFAs by creating eps-links
    -- from every terminal state of the second NDFA to the initial state of the first NDFA.
    --
    --    ╭───────╮        ╭───────╮
    --    │       │──E─┐   │       │──>
    -- ──>│ NDFA1 │──E─┼──>│ NDFA2 │──>
    --    │       │──E─┘   │       │──>
    --    ╰───────╯        ╰───────╯
    --
    compileInner (r1 :-: r2) ss = (ndfa, ss2)
      where
        (       ss0) = ss
        (ndfa1, ss1) = compileInner r1 ss0
        (ndfa2, ss2) = compileInner r2 ss1
        
        iState = initialState ndfa1
        tStates = terminalStates ndfa2
        aStates = allStates ndfa1 `Set.union` allStates ndfa2

        -- Links between 1st and 2nd NDFAs
        epsLinkTo2 = Map.fromList [(ELink, Set.fromList $ [initialState ndfa2])]
        linksBetween12 = Map.fromList [(state, epsLinkTo2) | state <- Set.toList $ terminalStates ndfa1]
        -- Links within the 1st NDFA
        linksWithin1 = innerLinks ndfa1
        -- Links within the 2st NDFA
        linksWithin2 = innerLinks ndfa2

        links = linksWithin1 `uniteLinks` linksWithin2 `uniteLinks` linksBetween12

        ndfa  = NDFA iState tStates aStates links
    -- Unification NDFA is formed of two sub-NDFAs by creating a single initial state
    -- and linking it with eps-links to the initial states of sub-NDFAs.
    --
    --              ╭───────╮
    --              │       │──>
    --          ┌──>│ NDFA1 │──>
    --          E   │       │──>
    --          │   ╰───────╯
    -- ──> () ──┤
    --          │   ╭───────╮
    --          E   │       │──>
    --          └──>│ NDFA2 │──>
    --              │       │──>
    --              ╰───────╯
    --
    compileInner (r1 :|: r2) (s:ss) = (ndfa, ss2)
      where
        (       ss0) = ss
        (ndfa1, ss1) = compileInner r1 ss0
        (ndfa2, ss2) = compileInner r2 ss1

        iState = State s
        tStates = (terminalStates ndfa1) `Set.union` (terminalStates ndfa2)
        aStates = allStates ndfa1 `Set.union` allStates ndfa2 `Set.union` (Set.fromList [iState])

        -- Link between iState and starts of the two NDFAs
        epsLinksTo = Map.fromList [(ELink, Set.fromList $ [initialState ndfa1, initialState ndfa2])]
        initialLinks = Map.fromList [(iState, epsLinksTo)]
        -- Links within the 1st NDFA
        linksWithin1 = innerLinks ndfa1
        -- Links within the 2st NDFA
        linksWithin2 = innerLinks ndfa2

        links = linksWithin1 `uniteLinks` linksWithin2 `uniteLinks` initialLinks

        ndfa  = NDFA iState tStates aStates links

    -- Epsilon NDFA consists of a single state.
    --
    -- ──> () ──>
    --
    compileInner (Eps) (s:ss) = (ndfa, ss)
      where
        iState = State s
        tStates = Set.fromList [iState]
        aStates = Set.fromList [iState]
        links = Map.empty

        ndfa  = NDFA iState tStates aStates links
    -- Iterative NDFA is formed of a sub-NDFA by linking it initial state
    -- to all it terminal states in both directions.
    --
    --   ┌───<─────<────┐
    --   │  ╭───────╮   │
    --   v  │       │─>─┼─>
    -- ──┼─>│ NDFA1 │─>─┼─>
    --   v  │       │─>─┼─>
    --   │  ╰───────╯   │
    --   └────>─────>───┘
    --
    compileInner (Ite r)     ss = (ndfa, ss1)
      where
        (       ss0) = ss
        (ndfa1, ss1) = compileInner r ss0

        iState = initialState ndfa1
        tStates = terminalStates ndfa1
        aStates = allStates ndfa1

        -- Links from initial state to terminal states
        epsLinksToT = Map.fromList [(ELink, tStates)]
        linksToT = Map.fromList [(iState, epsLinksToT)]
        -- Links from terminal states to initial state
        epsLinkToI = Map.fromList [(ELink, Set.fromList [iState])]
        linksToI = Map.fromList [(state, epsLinkToI) | state <- Set.toList $ tStates]
        -- Links within the 1st NDFA
        linksWithin1 = innerLinks ndfa1

        links = linksWithin1 `uniteLinks` linksToT `uniteLinks` linksToI

        ndfa  = NDFA iState tStates aStates links

    -- Make transitive clojure on eps-links, exchanging memory for speed.
    transitiveClojure :: NDFA -> NDFA
    transitiveClojure ndfa = NDFA iState tStates aStates (innerLinks ndfa `uniteLinks` allEpsLinks)
      where
        iState = initialState ndfa
        tStates = terminalStates ndfa
        aStates = allStates ndfa

        statesList = Set.toList $ allStates ndfa

        allEpsEdges = makeAllEdges $ Map.toList $ innerLinks $ ndfa
        allEpsLinks = Map.fromList [
          (node, Map.fromList [(ELink, to)]) |
            node <- statesList,
            let to = makeLinks node,
            Set.size to > 0]

        makeAllEdges []                = []
        makeAllEdges ((state, states):xs) = [(state, state2) | state2 <- to] ++ makeAllEdges xs
          where
            to = Set.toList $ fromMaybe Set.empty (states !? ELink)

        makeLinks node = Set.fromList $ filter (/=node) $ bfs statesList allEpsEdges [node]
        
        bfs []    _     _                                            = []
        bfs _     _     []                                           = []
        bfs nodes edges (current:stack) | not $ current `elem` nodes = bfs unwisited_nodes edges stack
                                        | otherwise = current : bfs unwisited_nodes edges (stack ++ neighbours)
            where
                neighbours = [x | (x, y) <- edges, y == current] ++ [x | (y, x) <- edges, y == current]
                unwisited_nodes = [x | x <- nodes, x /= current]

-- Evaluate given regexp on the given string. Return true if the string mathes this regexp.
evaluate :: NDFA -> String -> Bool
evaluate ndfa s = (Set.size $ Set.intersection evaluated (terminalStates ndfa)) > 0
  where
    evaluated = doEvaluate s (complementStates $ Set.fromList [initialState ndfa])

    links = innerLinks ndfa

    -- Given a set of states, yield a set of states that can be reached from original states
    -- following eps-links (original states included).
    -- We rely on links map being a transitive closure here.
    complementStates :: Set State -> Set State
    complementStates states = states >>= complementState
      where
        complementState state = (Set.singleton state) `Set.union` (fromMaybe Set.empty (links !? state !?? ELink))

    -- Given a single state and a character, yield all states that can be reached from the
    -- original state following appropriate character-links. The final set is complemented.
    evaluateStates :: Char -> Set State -> Set State
    evaluateStates s states = complementStates (states >>= \state -> fromMaybe Set.empty (links !? state !?? (CLink s)))

    -- Evaluate given regex on the given string.
    doEvaluate :: String -> Set State -> Set State
    doEvaluate [] states = states
    doEvaluate (x:xs) states = doEvaluate xs $ evaluateStates  x states


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


-- Some example program --------------------------------------------------------

doRegexp :: NDFA -> IO ()
doRegexp ndfa = do
  putStr "> "
  hFlush stdout

  line <- getLine
  putStrLn $ show $ evaluate ndfa line
  doRegexp ndfa

main = do
  putStrLn "Enter a regexp. For example, \"(C 'a' :-: Ite (C 'a' :|: C 'b')) :-: C 'b'\":"

  regexp <- readUntilSuccess "> " :: IO RegExp
  ndfa <- return $ compile regexp

  putStrLn "Now enter some lines for checking:"

  doRegexp ndfa
