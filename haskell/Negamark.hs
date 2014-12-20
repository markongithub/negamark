-- | Nodes in game treeS

module Negamark where
  import Data.List (sortBy)
  import Data.Function (on)
  import Data.List.Ordered (nubSortBy)
  import Data.Maybe
  import Data.Ord (comparing)
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
    compare (Heuristic _ _) (Win _) = LT
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

  boardsAfter :: NegamarkGameState a => a -> Int -> [a]
  boardsAfter board 0     = [board]
--  boardsAfter board depth = nubBy (\x y -> uniqueID x == uniqueID y) (concat (map allLegalMoves (boardsAfter board (depth - 1))))
  boardsAfter board depth = nubSortBy (comparing uniqueID) $ concat (map allLegalMoves (boardsAfter board (depth - 1)))

  firstPass :: NegamarkGameState a => a -> Outcome
  firstPass board | findWinner board == activePlayer board =
      Win (movesSoFar board)
  firstPass board | findWinner board == otherPlayer (activePlayer board) = 
      Loss (movesSoFar board)
  firstPass board | null (allLegalMoves board) =
      Stalemate (movesSoFar board)
  firstPass board | otherwise =
      Heuristic (movesSoFar board) (heuristicValue board)

  firstPassIO :: (NegamarkGameState a, TranspositionTable b) => a -> b -> IO(Outcome)
  firstPassIO board table = do
    if findWinner board == activePlayer board
        then return (Win (movesSoFar board))
        else if findWinner board == otherPlayer (activePlayer board)
             then return (Loss (movesSoFar board))
             else if null (allLegalMoves board)
                 then return (Stalemate (movesSoFar board))
                 else do fromCache <- getOutcome table (uniqueID board)
--                         putStrLn ("It came back " ++ show (fromCache))
                         return (fromMaybe (Heuristic (movesSoFar board) (heuristicValue board)) fromCache)
--                         if fromCache /= Nothing
--                             then return (fromJust fromCache)
--                             else return (Outcome Heuristic (movesSoFar board) (heuristicValue board))

  negamark :: NegamarkGameState a => a -> Int -> Outcome -> Outcome ->
               (Outcome, [a])
  negamark board depth alpha beta | traceNegamark board depth alpha beta False = undefined
  negamark board 0     alpha beta = (firstPass board, [board])
  negamark board depth alpha beta | isAuthoritative (firstPass board) (movesSoFar board + depth) = (firstPass board, [board])
  negamark board depth alpha beta | otherwise =
      (opposite(fst recursiveOutcome), (board:(snd recursiveOutcome)))
      where recursiveOutcome =
                   negamarkRecurse depth alpha beta (sortMovesByFirstPass (allLegalMoves board))

  negamarkIO :: (NegamarkGameState a, TranspositionTable t)  => a -> Int -> 
                Outcome -> Outcome -> t -> IO(Outcome, [a])
  negamarkIO board depth alpha beta table | traceNegamark board depth alpha beta False = undefined
  negamarkIO board depth alpha beta table = do
      fp <- firstPassIO board table
      if depth == 0
        then return (fp, [board])
        else if (isAuthoritative fp (movesSoFar board + depth))
          then return (fp, [board])
          else do
            sortedMoves <- sortMovesByFirstPassIO (allLegalMoves board) table
            recursiveOutcome <- negamarkRecurseIO depth alpha beta sortedMoves table
            saveOutcome table (uniqueID board) (opposite(fst recursiveOutcome))
            return (opposite(fst recursiveOutcome), (board:(snd recursiveOutcome)))

  sortMovesByFirstPass :: NegamarkGameState a => [a] -> [a]
  sortMovesByFirstPass boards =
      sortBy (compare `on` firstPass) boards
      
  compareIO :: (Ord a) => IO a -> IO a -> IO Ordering
  compareIO thing1 thing2 = do
    thing1value <- thing1
    thing2value <- thing2
    return (compare thing1value thing2value)


  sortMovesByFirstPassIO :: (NegamarkGameState a, TranspositionTable t) => [a] ->
                          t -> IO ([a])
  sortMovesByFirstPassIO boards table = do
    outcomes <- sequence (map (\x -> firstPassIO x table) boards)
    return (map fst (sortBy (compare `on` snd) (zip boards outcomes)))

  traceNegamark board depth alpha beta foo
      | depth < 16  = foo
      | otherwise  = trace ("nm  d " ++ show depth ++ " " ++ summary board ++ 
                            " a " ++  show alpha ++ " b " ++ show beta) foo

  negamarkRecurse :: NegamarkGameState a => Int -> Outcome -> Outcome -> [a] ->
       (Outcome, [a])
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
      | opposite(fst xOutcome) >= opposite(fst tailResult)    = xOutcome
--      | trace ("nmr d " ++ show depth ++ " a " ++ show alpha ++ " b " ++ show beta ++ " newA " ++ show newAlpha ++ " x " ++ show x ++ " returning tailResult because " ++ show (fst tailResult) ++ " is worse for the other guy than is " ++ show (fst xOutcome)) False = undefined
      | otherwise                         = tailResult
      where xOutcome =
             (negamark x (depth - 1) (opposite beta) (opposite alpha))
            tailResult =
             (negamarkRecurse depth newAlpha beta xs)
            newAlpha = max alpha (opposite (fst xOutcome))

  negamarkRecurseIO :: (NegamarkGameState a, TranspositionTable t) => Int ->
                       Outcome -> Outcome -> [a] -> t -> IO(Outcome, [a])
  negamarkRecurseIO depth alpha beta (x:xs) table = do
    if movesSoFar x >= maxMove table
      then do
        return (negamarkRecurse depth alpha beta (x:xs))
      else do
        xOutcome <- negamarkIO x (depth - 1) (opposite beta) (opposite alpha) table
        let newAlpha = max alpha (opposite (fst xOutcome))
        if (length xs == 0) || (newAlpha >= beta)
          then return xOutcome
          else do tailResult <- negamarkRecurseIO depth newAlpha beta xs table
                  if opposite(fst xOutcome) >= opposite(fst tailResult)
                    then return xOutcome
                    else return tailResult

  negamarkSimple :: NegamarkGameState a => a -> Int -> (Outcome, [a])
  negamarkSimple board depth =
      negamark board depth (Loss 0) (Win 0)

  proveIsLoss :: NegamarkGameState a => a -> Int -> Outcome
  proveIsLoss board depth = fst (negamark board depth (Loss 1000) (Loss 1001))

  proveIsWin :: NegamarkGameState a => a -> Int -> Outcome
  proveIsWin board depth = fst (negamark board depth (Win 1001) (Win 1000))

  pickMove :: NegamarkGameState a => a -> Int -> (Outcome, [a])
  pickMove board depth = result
      where result = (negamarkRecurse depth (Loss 37)
                      (Win 36) (sortMovesByFirstPass (allLegalMoves board)))

  proveIsLossIO :: (NegamarkGameState a, TranspositionTable t) =>
                   a -> Int -> t -> IO Outcome
  proveIsLossIO board depth table = do
      (outcome, _) <- negamarkIO board depth (Loss 1000) (Loss 1001) table
      return outcome

  proveIsWinIO :: (NegamarkGameState a, TranspositionTable t) =>
                   a -> Int -> t -> IO Outcome
  proveIsWinIO board depth table = do
      (outcome, _) <- negamarkIO board depth (Win 1001) (Win 1000) table
      return outcome

  pickMoveIO :: (NegamarkGameState a, TranspositionTable t) => a -> Int -> t -> IO(Outcome, [a])
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

  playGame :: NegamarkGameState a => a -> Bool -> Bool -> Int -> IO SquareState
  playGame board autoX autoO strength
      | length (allLegalMoves board) == 0 = do {return SquareOpen}
      | findWinner board /= SquareOpen    = do
            return (findWinner board)
      | isAutomated (activePlayer board) autoX autoO = do
            let result = pickMove board strength
            putStrLn ("The best you can do is " ++ show (fst result))
            ending <- playGame (head (snd result)) autoX autoO strength
            return ending
      | otherwise = do
            nextMove <- getHumanMove board
            ending <- playGame nextMove autoX autoO strength
            return ending

  playGameIO :: (NegamarkGameState a, TranspositionTable t) => a -> t -> IO SquareState
  playGameIO board table | length (allLegalMoves board) == 0 = do {return SquareOpen}
                         | findWinner board /= SquareOpen    = do
                               return (findWinner board)
                         | activePlayer board == X           = do
                               result <- pickMoveIO board 10 table
                               putStrLn ("The best you can do is " ++ show (fst result))
                               ending <- playGameIO (head (snd result)) table
                               return ending
                         | otherwise = do
                               nextMove <- getHumanMove board
                               ending <- playGameIO nextMove table
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

