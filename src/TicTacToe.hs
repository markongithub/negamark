-- | People in other countries call it Aughts and Crosses or something

module TicTacToe where
  import Data.Array
  import Negamark

  data TicTacToeBoardState = TicTacToeBoardState
      { squares      :: (Array Integer SquareState)
      , activePlayerTTT :: SquareState
      , movesSoFarTTT   :: Int
      } deriving (Show)

  instance NegamarkGameState TicTacToeBoardState where
    activePlayer board = activePlayerTTT board
    movesSoFar board = movesSoFarTTT board
    heuristicValue board = 0 -- in the ghetto... IN THE GHETTO...
    uniqueID board = uniqueIDTTT board
    summary board = "whatever"
    findWinner board = findTicTacToeWinner board waysToWin
    allLegalMoves board = 
        map (\m -> newTicTacToeStateFromMove m board) (availableMoves board)
    getHumanMove board = getHumanTicTacToeMove board

  squareDigit SquareOpen = 0
  squareDigit X = 1
  squareDigit O = 2

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

  uniqueIDTTT board = let
    valueForSquare i = (10 ^ (8 - i)) * (squareDigit $ squareState board i)
    in sum $ map valueForSquare [0..8]

  findTicTacToeWinner ::
      TicTacToeBoardState -> [(Integer, Integer, Integer)] -> SquareState
  findTicTacToeWinner board [] = SquareOpen
  findTicTacToeWinner board (x:xs) = if (valueFromSquares board x) /= SquareOpen
                               then valueFromSquares board x
                               else findTicTacToeWinner board xs
  newTicTacToeStateFromMove ::
      Integer -> TicTacToeBoardState -> TicTacToeBoardState
  newTicTacToeStateFromMove move board =
      TicTacToeBoardState ((squares board) // [(move, activePlayerTTT board)])
                          (otherPlayer(activePlayerTTT board))
                          ((movesSoFarTTT board) + 1)

  children :: TicTacToeBoardState -> [TicTacToeBoardState]
  children board = 
      map (\m -> newTicTacToeStateFromMove m board) (availableMoves board)

  getHumanTicTacToeMove :: TicTacToeBoardState -> IO TicTacToeBoardState
  getHumanTicTacToeMove board = do
      destination <- getHumanDestination board
      return (newTicTacToeStateFromMove destination board)

  getHumanDestination :: TicTacToeBoardState -> IO Integer
  getHumanDestination board = do
    putStrLn (show board)
    putStrLn "Make your move. If it's invalid in any way we will crash."
    number <- getLine
    return (read number :: Integer)

  startGame = do
    playGame newTicTacToeBoard

