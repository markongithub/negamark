-- | Nodes in game trees

module Negamark where
  import Array
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

  type TicTacToeMove = Int 

  data TicTacToeBoardState = TicTacToeBoardState
      { squares      :: (Array Integer SquareState)
      , activePlayer :: SquareState
      , movesSoFar   :: Int
      } deriving (Show)

  squareState board i = (squares board)!i
  squareAvailable board i = squareState board i == SquareOpen
  availableMoves board = filter (squareAvailable board) [0..8]
  newTicTacToeBoard =
      TicTacToeBoardState (array (0,8) [(i, SquareOpen) | i <- [0..8]]) X 0
  waysToWin = [(0, 1, 2), (0, 3, 6), (0, 4, 8), (1, 4, 7), (2, 4, 6),
               (2, 5, 8), (3, 4, 5), (6, 7, 8)]
  valueFromSquares board (i1, i2, i3) = let square = squares board
      in if square!i1 == square!i2 && square!i1 == square!i3 then square!i1
         else SquareOpen
  findWinner ::
      TicTacToeBoardState -> [(Integer, Integer, Integer)] -> SquareState
  findWinner board [] = SquareOpen
  findWinner board (x:xs) = if (valueFromSquares board x) /= SquareOpen
                               then valueFromSquares board x
                               else findWinner board xs
  newTicTacToeStateFromMove ::
      Integer -> TicTacToeBoardState -> TicTacToeBoardState
  newTicTacToeStateFromMove move board =
      TicTacToeBoardState ((squares board) // [(move, activePlayer board)])
                          (otherPlayer(activePlayer board))
                          ((movesSoFar board) + 1)
  children board = 
      map (\m -> newTicTacToeStateFromMove m board) (availableMoves board)

  firstPass board | findWinner board waysToWin == activePlayer board =
      Outcome Win (movesSoFar board) 1
  firstPass board | findWinner board waysToWin == 
                    otherPlayer (activePlayer board) = 
      Outcome Loss (movesSoFar board) 0
  firstPass board | length (availableMoves board) == 0 =
      Outcome Stalemate (movesSoFar board) 0
  firstPass board | otherwise = Outcome Heuristic (movesSoFar board) 0

  negamark :: TicTacToeBoardState -> Int -> Outcome -> Outcome -> Outcome
  negamark board 0 _ _ = firstPass board
  negamark board depth _ _ | value(firstPass board) /= Heuristic = firstPass board
  negamark board depth alpha beta | otherwise =
--      opposite (minimum (map (\b -> negamark b (depth -1)) (children board)))
--      opposite (minChildOutcome (depth - 1) (children board))
--      negamarkRecurse (depth - 1) (Outcome Loss 0 0) (Outcome Win 0 0) (children board)
      negamarkRecurse (depth - 1) alpha beta board (availableMoves board)

  negamarkRecurse :: Int -> Outcome -> Outcome -> TicTacToeBoardState ->
                     [Integer] -> Outcome
  negamarkRecurse depth alpha beta board [] = Outcome Loss 0 0 -- SO WRONG
  negamarkRecurse depth alpha beta board (x:xs) =
      let thisChild = opposite (negamark (newTicTacToeStateFromMove x board) depth (opposite beta) (opposite alpha))
      in if thisChild >= beta then thisChild
      else maximum [thisChild, negamarkRecurse depth (maximum[thisChild, alpha]) beta board xs]

  getHumanTicTacToeMove board = do
    putStrLn (show board)
    putStrLn "Make your move. If it's invalid in any way we will crash."
    number <- getLine
    return (read number :: Integer)

  playGame board = do
    if length (availableMoves board) > 0
       then do nextMove <- getHumanTicTacToeMove board
               playGame (newTicTacToeStateFromMove nextMove board)
       else return True

  startGame = do
    playGame newTicTacToeBoard

