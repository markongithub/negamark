-- | Nodes in game trees

module Negamark where
  import Array
  import Data.List
  import Debug.Trace
  import IO

  data OutcomeValue = Loss | Stalemate | Heuristic | Win
                      deriving (Show, Eq, Ord)

  data Outcome = Outcome OutcomeValue Int Int
                 deriving (Eq) 

  value (Outcome a b c) = a
  depth (Outcome a b c) = b
  heuristic (Outcome a b c) = c

  opposite :: Outcome -> Outcome
  opposite (Outcome Loss depth _) = Outcome Win depth 0
  opposite (Outcome Win depth _) = Outcome Loss depth 0
  opposite (Outcome Heuristic depth heuristic) =
      Outcome Heuristic depth (-1 * heuristic)
  opposite (Outcome Stalemate depth _) = Outcome Stalemate depth 0

  instance Show Outcome where
    show (Outcome v d h)
        | v == Heuristic = show v ++ show h ++ "@" ++ show d
        | otherwise      = show v ++ "@" ++ show d

  instance Ord Outcome where
    compare (Outcome v1 d1 h1) (Outcome v2 d2 h2)
        | v1 /= v2        = compare v1 v2
        | v1 == Win       = compare d2 d1
        | v1 == Heuristic = compare h1 h2
        | otherwise       = compare d1 d2

  data SquareState = X | O | SquareOpen
                     deriving (Eq, Show)
  otherPlayer X = O
  otherPlayer O = X

  endingDescription :: SquareState -> [Char]
  endingDescription SquareOpen = "We'll call it a draw."
  endingDescription player = (show player ++ " is the winner.")
  
  class NegamarkGameState gameState where
    activePlayer :: gameState -> SquareState
    movesSoFar :: gameState -> Int
    heuristicValue :: gameState -> Int
    findWinner :: gameState -> SquareState
    allLegalMoves :: gameState -> [gameState]
    getHumanMove :: gameState -> IO gameState
    summary :: gameState -> [Char]
    uniqueID :: gameState -> Integer
--    transpositionTable :: gameState -> TranspositionTable

  firstPass :: NegamarkGameState a => a -> Outcome
  firstPass board | findWinner board == activePlayer board =
      Outcome Win (movesSoFar board) 1
  firstPass board | findWinner board == otherPlayer (activePlayer board) = 
      Outcome Loss (movesSoFar board) 0
  firstPass board | null (allLegalMoves board) =
      Outcome Stalemate (movesSoFar board) 0
--  firstPass board | savedOutcome board /= Nothing = fromJust(savedOutcome board)
  firstPass board | otherwise =
      Outcome Heuristic (movesSoFar board) (heuristicValue board)

  negamark :: NegamarkGameState a => a -> Int -> Outcome -> Outcome ->
               (Outcome, [a])
  negamark board depth alpha beta | traceNegamark board depth alpha beta False = undefined
  negamark board 0     alpha beta = (firstPass board, [board])
  negamark board depth alpha beta | value(firstPass board) /= Heuristic =
      (firstPass board, [board])
  negamark board depth alpha beta | otherwise =
      (opposite(fst recursiveOutcome), (board:(snd recursiveOutcome)))
      where recursiveOutcome =
                   negamarkRecurse depth alpha beta (sortBy (\x y -> compare (firstPass x) (firstPass y)) (allLegalMoves board))

  traceNegamark board depth alpha beta foo
      | depth < 99  = foo
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

  negamarkSimple :: NegamarkGameState a => a -> Int -> (Outcome, [a])
  negamarkSimple board depth =
      negamark board depth (Outcome Loss 0 0) (Outcome Win 0 0)

  pickMove :: NegamarkGameState a => a -> Int -> (Outcome, [a])
  pickMove board depth = result
      where result = (negamarkRecurse depth (Outcome Loss 0 0)
                      (Outcome Win 0 0) (allLegalMoves board))

  playGame :: NegamarkGameState a => a -> IO SquareState
  playGame board | length (allLegalMoves board) == 0 = do {return SquareOpen}
                 | findWinner board /= SquareOpen    = do
                       return (findWinner board)
                 | activePlayer board == X           = do
                       let result = pickMove board 5
                       putStrLn ("The best you can do is " ++ show (fst result))
                       ending <- playGame (head (snd result))
                       return ending
                 | otherwise = do
                       nextMove <- getHumanMove board
                       ending <- playGame nextMove
                       return ending

  class TranspositionTable table where
    getOutcome :: table -> Maybe Outcome

  data NullTranspositionTable = NullTranspositionTable

  instance TranspositionTable NullTranspositionTable where
    getOutcome nullTable  = Nothing

