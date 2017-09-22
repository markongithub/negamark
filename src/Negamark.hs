module Negamark where
  import Control.Monad (liftM)
  import Data.List (sortBy)
  import Data.Function (on)
  import qualified Data.Map as Map
  import Data.Maybe
--  for boardsAfter -- import Data.Ord (comparing)
  import Debug.Trace

  type MoveNumber = Int
  type HeuristicValue = Int

  data Outcome = Win MoveNumber | Stalemate MoveNumber | Loss MoveNumber
               | Heuristic MoveNumber HeuristicValue
                 deriving (Eq)

  opposite :: Outcome -> Outcome
  opposite (Loss depth) = Win depth
  opposite (Win depth) = Loss depth
  opposite (Heuristic depth heuristic) = Heuristic depth (-1 * heuristic)
  opposite (Stalemate depth) = Stalemate depth

  instance Show Outcome where
    show (Win depth) = "Win@" ++ show depth
    show (Loss depth) = "Loss@" ++ show depth
    show (Stalemate depth) = "Stalemate@" ++ show depth
    show (Heuristic depth heuristic) =
        "Heuristic" ++ show heuristic ++ "@" ++ show depth

  instance Ord Outcome where
    compare (Win d1) (Win d2) = compare d2 d1
    compare (Win d1) whatever = GT
    compare (Heuristic d1 h1) (Heuristic d2 h2)
      | h1 /= h2  = compare h1 h2
      | otherwise = compare d1 d2
    compare (Heuristic _ _) whatever = GT
    compare (Stalemate _) (Win _) = LT
    compare (Stalemate _) (Heuristic _ _) = LT
    compare (Stalemate d1) (Stalemate d2) = compare d1 d2
    compare (Stalemate _) whatever = GT
    compare (Loss d1) (Loss d2) = compare d1 d2
    compare (Loss _) whatever = LT

  isAuthoritative :: Outcome -> Int -> Bool
  isAuthoritative (Heuristic depth _) searchDepth = depth >= searchDepth
  isAuthoritative _ _ = True

  data SquareState = X | O | SquareOpen
                     deriving (Eq, Show)
  otherPlayer X = O
  otherPlayer O = X

  endingDescription :: SquareState -> [Char]
  endingDescription SquareOpen = "We'll call it a draw."
  endingDescription player = (show player ++ " is the winner.")

  class NegamarkGameState gameState where
    activePlayer :: gameState -> SquareState
    movesSoFar :: gameState -> MoveNumber
    heuristicValue :: gameState -> HeuristicValue
    findWinner :: gameState -> SquareState
    allLegalMoves :: gameState -> [gameState]
    getHumanMove :: gameState -> IO gameState
    summary :: gameState -> [Char]
    uniqueID :: gameState -> Integer

--  boardsAfter :: NegamarkGameState a => a -> Int -> [a]
--  boardsAfter board 0     = [board]
--  boardsAfter board depth = nubBy (\x y -> uniqueID x == uniqueID y) (concat (map allLegalMoves (boardsAfter board (depth - 1))))
--  boardsAfter board depth = nubSortBy (comparing uniqueID) $ concat (map allLegalMoves (boardsAfter board (depth - 1)))

  firstPass :: NegamarkGameState a => a -> Outcome
  firstPass board | findWinner board == activePlayer board =
      Win (movesSoFar board)
  firstPass board | findWinner board == otherPlayer (activePlayer board) =
      Loss (movesSoFar board)
  firstPass board | null (allLegalMoves board) =
      Stalemate (movesSoFar board)
  firstPass board | otherwise =
      Heuristic (movesSoFar board) (heuristicValue board)

  firstPassIO :: (NegamarkGameState a, TranspositionTable b) =>
                 a -> b -> IO(Outcome)
  firstPassIO board table =
    let pureFP = firstPass board
        queryResult = getOutcome table (uniqueID board)
        fromTable = liftM (fromMaybe pureFP) queryResult
-- We only look at the table if the pure firstPass returns a Heuristic.
    in case pureFP of (Heuristic _ _) -> fromTable
                      otherwise       -> return pureFP

  type TranspositionMap = Map.Map Integer Outcome
  noMap = Map.empty

  data NegamarkResult a = NegamarkResult {
      resultOutcome :: Outcome
    , resultMoves   :: [a]
    , resultMap     :: TranspositionMap
    }

  storeTransposition :: (NegamarkGameState a) => TranspositionMap -> Outcome -> a -> TranspositionMap
  storeTransposition tMap o board = Map.insert (uniqueID board) o tMap

  negamark :: NegamarkGameState a => a -> Int -> Outcome -> Outcome ->
              NegamarkResult a
  negamark board depth alpha beta | traceNegamark board depth alpha beta "none" False = undefined
  negamark board depth alpha beta
    | depth == 0 = justFirstPass
    | isAuthoritative (firstPass board) (movesSoFar board + depth) = justFirstPass
    | otherwise =
      NegamarkResult (opposite(resultOutcome recursiveOutcome)) (board:(resultMoves recursiveOutcome)) noMap
      where recursiveOutcome =
                   negamarkRecurse depth alpha beta (sortMovesByFirstPass (allLegalMoves board))
            justFirstPass = NegamarkResult (firstPass board) [board] noMap

  negamarkIO :: (NegamarkGameState a, TranspositionTable t)  => a -> Int ->
                Outcome -> Outcome -> t -> IO(NegamarkResult a)
  negamarkIO board depth alpha beta table | traceNegamark board depth alpha beta "none" False = undefined
  negamarkIO board depth alpha beta table = do
      fp <- firstPassIO board table
      if depth == 0
        then return $ NegamarkResult fp [board] noMap
        else if (isAuthoritative fp (movesSoFar board + depth))
          then return $ NegamarkResult fp [board] noMap
          else do
            sortedMoves <- sortMovesByFirstPassIO (allLegalMoves board) table
            recursiveOutcome <- negamarkRecurseIO depth alpha beta sortedMoves table
            saveOutcome table (uniqueID board) (opposite(resultOutcome recursiveOutcome))
            return $ NegamarkResult (opposite(resultOutcome recursiveOutcome)) (board:(resultMoves recursiveOutcome)) noMap

  sortMovesByFirstPass :: NegamarkGameState a => [a] -> [a]
  sortMovesByFirstPass boards =
      sortBy (compare `on` firstPass) boards

  sortMovesByFirstPassIO :: (NegamarkGameState a, TranspositionTable t) =>
                            [a] -> t -> IO ([a])
  sortMovesByFirstPassIO boards table = do
    outcomes <- sequence (map (\x -> firstPassIO x table) boards)
    return (map fst (sortBy (compare `on` snd) (zip boards outcomes)))

  traceNegamark board depth alpha beta mapSize foo
      | depth < 14  = foo
      | otherwise  = trace ("nm  d " ++ show depth ++ " " ++ summary board ++
                            " a " ++  show alpha ++ " b " ++ show beta ++ " ms " ++ mapSize) foo

  negamarkRecurse :: NegamarkGameState a => Int -> Outcome -> Outcome -> [a] ->
                                            NegamarkResult a
  negamarkRecurse depth alpha beta [] = error "maximum of empty list"
  negamarkRecurse depth alpha beta (x:xs)
--     | trace ("nmr d " ++ show depth ++ " " ++ summary x ++ " a " ++ show alpha ++ " b " ++ show beta ++ " with " ++ show (length xs) ++ " more to go") False = undefined
--      | (alpha > beta) && (value alpha /= Stalemate) &&
--        (value beta /= Stalemate)         = error ("alpha " ++ show alpha ++ " > beta " ++ show beta)
      | length xs == 0                    = xOutcome
--      | trace (summary x ++ " could achieve " ++ show (opposite (fst xOutcome)) ++ " and beta is " ++ show beta ++ " - should we prune?") False = undefined
      | newAlpha >= beta                  = xOutcome
--      | trace "We did not prune. Now we may return xOutcome." False = undefined
--      | value (opposite(fst xOutcome)) == Win    = xOutcome
      | opposite(resultOutcome xOutcome) >= opposite(resultOutcome tailResult)    = xOutcome
--      | trace ("nmr d " ++ show depth ++ " a " ++ show alpha ++ " b " ++ show beta ++ " newA " ++ show newAlpha ++ " x " ++ show x ++ " returning tailResult because " ++ show (fst tailResult) ++ " is worse for the other guy than is " ++ show (fst xOutcome)) False = undefined
      | otherwise                         = tailResult
      where xOutcome =
             (negamark x (depth - 1) (opposite beta) (opposite alpha))
            tailResult =
             (negamarkRecurse depth newAlpha beta xs)
            newAlpha = max alpha (opposite (resultOutcome xOutcome))

  negamarkRecurseIO :: (NegamarkGameState a, TranspositionTable t) => Int ->
                       Outcome -> Outcome -> [a] -> t -> IO(NegamarkResult a)
  negamarkRecurseIO depth alpha beta (x:xs) table = do
    if movesSoFar x >= maxMove table
      then do
        return (negamarkRecurse depth alpha beta (x:xs))
      else do
        xOutcome <- negamarkIO x (depth - 1) (opposite beta) (opposite alpha) table
        let newAlpha = max alpha (opposite (resultOutcome xOutcome))
        if (length xs == 0) || (newAlpha >= beta)
          then return xOutcome
          else do tailResult <- negamarkRecurseIO depth newAlpha beta xs table
                  if opposite(resultOutcome xOutcome) >= opposite(resultOutcome tailResult)
                    then return xOutcome
                    else return tailResult

  negamarkSimple :: NegamarkGameState a => a -> Int -> NegamarkResult a
  negamarkSimple board depth =
      negamark board depth (Loss 0) (Win 0)

  negamarkSimple3 :: NegamarkGameState a => a -> Int -> NegamarkResult a
  negamarkSimple3 board depth =
      negamark3 board depth (Loss 0) (Win 0) noMap

  proveIsLoss :: NegamarkGameState a => a -> Int -> Outcome
  proveIsLoss board depth = resultOutcome (negamark board depth (Loss 1000) (Loss 1001))

  proveIsLoss3 :: NegamarkGameState a => a -> Int -> Outcome
  proveIsLoss3 board depth = resultOutcome (negamark3 board depth (Loss 1000) (Loss 1001) noMap)

  proveIsWin :: NegamarkGameState a => a -> Int -> Outcome
  proveIsWin board depth = resultOutcome (negamark board depth (Win 1001) (Win 1000))

  pickMove :: NegamarkGameState a => a -> Int -> NegamarkResult a
  pickMove board depth = result
      where result = (negamarkRecurse depth (Loss 37)
                      (Win 36) (sortMovesByFirstPass (allLegalMoves board)))

  proveIsLossIO :: (NegamarkGameState a, TranspositionTable t) =>
                   a -> Int -> t -> IO Outcome
  proveIsLossIO board depth table =
      liftM resultOutcome $ negamarkIO board depth (Loss 1000) (Loss 1001) table

  proveIsWinIO :: (NegamarkGameState a, TranspositionTable t) =>
                   a -> Int -> t -> IO Outcome
  proveIsWinIO board depth table =
      liftM resultOutcome $ negamarkIO board depth (Win 1001) (Win 1000) table

  pickMoveIO :: (NegamarkGameState a, TranspositionTable t) =>
                a -> Int -> t -> IO(NegamarkResult a)
  pickMoveIO board depth table
    | movesSoFar board >= maxMove table = return (pickMove board depth)
    | otherwise  = do
        sortedMoves <- sortMovesByFirstPassIO (allLegalMoves board) table
        negamarkRecurseIO depth (Loss 38) (Win 37) sortedMoves table

  isAutomated :: SquareState -> Bool -> Bool -> Bool
  isAutomated player autoX autoO
      | player == X && autoX = True
      | player == O && autoO = True
      | otherwise            = False

  playGame :: (NegamarkGameState a, Show a) => a -> Bool -> Bool -> Int -> IO SquareState
  playGame board autoX autoO strength
      | length (allLegalMoves board) == 0 = do
            putStrLn (show board)
            return SquareOpen
      | findWinner board /= SquareOpen    = do
            putStrLn (show board)
            return (findWinner board)
      | isAutomated (activePlayer board) autoX autoO = do
            let result = pickMove board strength
            putStrLn $ formatResult result
            ending <- playGame (head (resultMoves result)) autoX autoO strength
            return ending
      | otherwise = do
            nextMove <- getHumanMove board
            ending <- playGame nextMove autoX autoO strength
            return ending

  formatResult :: NegamarkGameState a => NegamarkResult a -> String
  formatResult result = let
    playerName = show $ activePlayer $ head $ resultMoves result
    outcomeName = show $ resultOutcome result
    in "The best " ++ playerName ++ " can do is " ++ outcomeName

  playGameIO :: (NegamarkGameState a, Show a, TranspositionTable t) => a -> Bool -> Bool -> Int -> t -> IO SquareState
  playGameIO board autoX autoO strength table
    | length (allLegalMoves board) == 0 = do
        putStrLn (show board)
        return SquareOpen
    | findWinner board /= SquareOpen    = do
        putStrLn (show board)
        return (findWinner board)
    | isAutomated (activePlayer board) autoX autoO = do
        result <- pickMoveIO board strength table
        putStrLn $ formatResult result
        ending <- playGameIO (head (resultMoves result)) autoX autoO strength table
        return ending
    | otherwise = do
        nextMove <- getHumanMove board
        ending <- playGameIO nextMove autoX autoO strength table
        return ending

  class TranspositionTable table where
    getOutcome :: table -> Integer -> IO (Maybe Outcome)
    saveOutcome :: table -> Integer -> Outcome -> IO ()
    maxMove :: table -> Int

  data NullTranspositionTable = NullTranspositionTable

  instance TranspositionTable NullTranspositionTable where
    getOutcome nullTable state = do
      return Nothing
    saveOutcome nullTable state outcome = do
      return ()
    maxMove nullTable = 0

  negamark3 :: NegamarkGameState a => a -> Int -> Outcome -> Outcome -> TranspositionMap ->
               NegamarkResult a
  negamark3 board depth alpha beta tMap | traceNegamark board depth alpha beta (show $ Map.size tMap) False = undefined
  negamark3 board depth alpha beta tMap
    | depth == 0 = justFirstPass
    | isAuthoritative fp (movesSoFar board + depth) = justFirstPass
    | otherwise =
      NegamarkResult (opposite(resultOutcome recursiveOutcome)) (board:(resultMoves recursiveOutcome)) newMap
      where fp = firstPass3 board tMap
            recursiveOutcome =
                   negamark3Iterate depth alpha beta (sortMovesByFirstPass (allLegalMoves board)) tMap
            justFirstPass = NegamarkResult fp [board] (storeTransposition tMap fp board)
            newMap = Map.union (resultMap recursiveOutcome) tMap

  firstPass3 :: NegamarkGameState a => a -> TranspositionMap -> Outcome
  firstPass3 board tMap = case Map.lookup (uniqueID board) tMap of
    Just o -> o
    _      -> firstPass board

  negamark3Iterate :: NegamarkGameState a => Int -> Outcome -> Outcome -> [a] -> TranspositionMap ->
                                            NegamarkResult a
  negamark3Iterate depth alpha beta [] _ = error "maximum of empty list?"
  negamark3Iterate depth alpha beta (x:xs) tMap
--     | trace ("nm3i d " ++ show depth ++ " " ++ summary x ++ " a " ++ show alpha ++ " b " ++ show beta ++ " map " ++ show (Map.size tMap) ++ " with " ++ show (length xs) ++ " more to go") False = undefined
--      | (alpha > beta) && (not $ isStalemate alpha) &&
--        (not $ isStalemate beta)         = error ("alpha " ++ show alpha ++ " > beta " ++ show beta)
      | null xs                           = summaryResult
--      | trace (summary x ++ " could achieve " ++ show (opposite (resultOutcome xResult)) ++ " and beta is " ++ show beta ++ " - should we prune?") False = undefined
      | alphaForTail >= beta                  = xResult
--      | trace "We did not prune. Now we may return xResult." False = undefined
      | opposite(resultOutcome xResult) >= opposite(resultOutcome tailResult)    = xResult
--      | trace ("nmr d " ++ show depth ++ " a " ++ show alpha ++ " b " ++ show beta ++ " newA " ++ show alphaForTail ++ " returning tailResult because " ++ show (resultOutcome tailResult) ++ " is worse for the other guy than is " ++ show (resultOutcome xResult)) False = undefined
      | otherwise                         = tailResult
      where xResult =
             (negamark3 x (depth - 1) (opposite beta) (opposite alpha) tMap)
            tailResult =
             (negamark3Iterate depth alphaForTail beta xs tMapForTail)
            alphaForTail = max alpha (opposite (resultOutcome xResult))
            tMapForTail = Map.union (resultMap xResult) tMap
            summaryResult = xResult { resultMap = tMapForTail }
