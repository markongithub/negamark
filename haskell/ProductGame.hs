-- | People in other countries call it Aughts and Crosses or something

module ProductGame where
  import Array
  import qualified Data.Map as Map
  import IO
  import List
  import Negamark

  height = 6
  width = 6

  type SquareXY = (Int, Int)
  type SquareArray = (Array Int (Array Int SquareState))
  type FactorPair = (Int, Int)
  type WinScoreArray = (Array Int (Array Int Int))
  type QuickScoreArray = (Array Int Int)

  data ProductGameState = ProductGameState
      { squares      :: SquareArray
      , activePlayerPG :: SquareState
      , movesSoFarPG   :: Int
      , topFactor   :: Int
      , bottomFactor   :: Int
      , winScores   :: WinScoreArray
      , quickWinScores :: QuickScoreArray
      , summaryPG :: [Char]
      , uniqueIDPG :: Integer
      }

  instance NegamarkGameState ProductGameState where
    activePlayer board = activePlayerPG board
    movesSoFar board = movesSoFarPG board
    heuristicValue board =
        ((quickWinScores board)!(playerIndex (activePlayer board)) -
         (quickWinScores board)!(playerIndex (
            otherPlayer (activePlayer board))))
    findWinner board
        | heuristicValue board >= 1000  = activePlayerPG board
        | heuristicValue board <= -1000 = otherPlayer(activePlayerPG board)
        | otherwise                     = SquareOpen
    allLegalMoves board = 
        map (\m -> newProductGameStateFromMove m board) (availableMoves board)
    getHumanMove board = getHumanProductGameMove board
    summary board = summaryPG board
    uniqueID board = uniqueIDPG board

  forwardLookupTable = array (0,5) [
      (0,array (0,5) [(0, 1),(1, 2),(2, 3),(3, 4),(4, 5),(5, 6)]),
      (1,array (0,5) [(0, 7),(1, 8),(2, 9),(3,10),(4,12),(5,14)]),
      (2,array (0,5) [(0,15),(1,16),(2,18),(3,20),(4,21),(5,24)]),
      (3,array (0,5) [(0,25),(1,27),(2,28),(3,30),(4,32),(5,35)]),
      (4,array (0,5) [(0,36),(1,40),(2,42),(3,45),(4,48),(5,49)]),
      (5,array (0,5) [(0,54),(1,56),(2,63),(3,64),(4,72),(5,81)])]

  allTuples = concat (map (\y -> (map (\x -> (y, x)) [0..5])) [0..5])
  reverseLookupTable = Map.fromList (map (\x -> (
      forwardLookupTable!(fst x)!(snd x), (fst x, snd x))) allTuples)

  productCoords :: Int -> SquareXY
  productCoords product =
      Map.findWithDefault (error "Invalid square.") product reverseLookupTable

  factorsToCoords :: (Int, Int) -> SquareXY
  factorsToCoords (x, y) = productCoords(x * y)

  squareState :: SquareArray -> Int -> Int -> SquareState
  squareState squares row column = squares!row!column

  squareAvailable :: ProductGameState -> SquareXY -> Bool
  squareAvailable board (row,column) =
      squareState (squares board) row column == SquareOpen

  showSquare :: SquareArray -> SquareXY -> [Char]
  showSquare squares (r,c)
      | squareState squares r c == SquareOpen =
          (take 3 (show (forwardLookupTable!r!c) ++ "  "))
      | otherwise = show (squareState squares r c) ++ "  "

  showRow :: SquareArray -> Int -> [Char]
  showRow board row =
      concat (map (\x -> showSquare board (row,x)) [0..(width - 1)])

  instance Show ProductGameState where
      show board =
          ("\n" ++
           concat (map (\x -> (showRow (squares board) x) ++ "\n") [0..(height - 1)]) ++
           "Top factor is on " ++ show (topFactor board) ++ 
           "\nBottom factor is on " ++ show (bottomFactor board) ++ 
           "\nThis is move " ++ show ((movesSoFar board) + 1) ++
           ". It is " ++ show (activePlayer board) ++ "'s turn.")

  smallerFirst :: (Int, Int) -> (Int, Int)
  smallerFirst (x, y) | x > y     = (y, x)
  smallerFirst (x, y) | otherwise = (x, y)

  potentialMoves :: Int -> [(Int, Int)]
  potentialMoves factor = map (\x -> (smallerFirst(factor, x))) [1..9]

  allPotentialMoves :: ProductGameState -> [(Int, Int)]
  allPotentialMoves board | topFactor board == 0 =
      nub (concat (map potentialMoves [1..9]))
  allPotentialMoves board = nub (concat [potentialMoves (topFactor board),
                                         potentialMoves (bottomFactor board)])

  availableMoves :: ProductGameState -> [(Int, Int)]
  availableMoves board =
      filter (\x -> squareAvailable board (factorsToCoords x))
             (allPotentialMoves board)

  oneDArray :: Int -> a -> (Array Int a)
  oneDArray size defaultValue = array (0, (size - 1))
                                      [(i, defaultValue) | i <- [0..(size - 1)]]

  twoDArray :: Int-> Int -> a -> (Array Int (Array Int a))
  twoDArray rows columns defaultValue =
      oneDArray rows (oneDArray columns defaultValue) 

  appendToListXY :: Int -> (Int, Int) -> (Array Int (Array Int [Int])) ->  
                    (Array Int (Array Int [Int]))
  appendToListXY value index matrix =
      matrix // [(row, matrix!row // [(column, value:matrix!row!column)])]
      where (row, column) = index

  setWinOnTetrad :: Int -> [SquareXY] -> (Array Int (Array Int [Int])) ->
                    (Array Int (Array Int [Int]))
  setWinOnTetrad winID indices matrix =
      foldr (appendToListXY winID) matrix indices

  setListOfTetrads :: [[SquareXY]] -> Int -> (Array Int (Array Int [Int])) ->
                      (Array Int (Array Int [Int]))
  setListOfTetrads [] _ matrix   = matrix
  setListOfTetrads (x:xs) winID matrix =
      setListOfTetrads xs (winID + 1) (setWinOnTetrad winID x matrix)
 
  leftToRightWins = [(r,c) | r <- [0..(height - 1)], c <- [0..(width - 4)]]
  goRight :: SquareXY -> SquareXY
  goRight tuple = (fst tuple, snd tuple + 1)

  downwardWins = [(r,c) | r <- [0..(height - 4)], c <- [0..(width - 1)]]
  goDown :: SquareXY -> SquareXY
  goDown tuple = (fst tuple + 1, snd tuple)

  downRightWins = [(r,c) | r <- [0..(height - 4)], c <- [0..(width - 4)]]
  goDownRight :: SquareXY -> SquareXY
  goDownRight tuple = (fst tuple + 1, snd tuple + 1)

  downLeftWins = [(r,c) | r <- [0..(height - 4)], c <- [(4 - 1)..(width - 1)]]
  goDownLeft :: SquareXY -> SquareXY
  goDownLeft tuple = (fst tuple + 1, snd tuple - 1)

  allIndicesForTetrad :: (SquareXY -> SquareXY) -> SquareXY ->
                         [SquareXY]
  allIndicesForTetrad op tuple =
     take 4 (iterate op tuple)

  allTetradsForDirection :: [SquareXY] -> (SquareXY -> SquareXY) ->
                            [[SquareXY]]
  allTetradsForDirection wins op = map (allIndicesForTetrad op) wins

  allWinningTetrads :: [[SquareXY]]
  allWinningTetrads = concat [allTetradsForDirection leftToRightWins goRight,
                              allTetradsForDirection downwardWins goDown,
                              allTetradsForDirection downRightWins goDownRight,
                              allTetradsForDirection downLeftWins goDownLeft]

  potentialWinMatrix :: (Array Int (Array Int [Int]))
  potentialWinMatrix =
      setListOfTetrads allWinningTetrads 0 (twoDArray height width [])

  winIDsForSquare :: SquareXY -> [Int]
  winIDsForSquare (row, col) = potentialWinMatrix!row!col 

  initialWinScores :: WinScoreArray
  initialWinScores = twoDArray 2 (length allWinningTetrads) 1

  playerIndex :: SquareState -> Int
  playerIndex X = 1
  playerIndex O = 0
  playerIndex _ = error "ugh"

  reversePlayerIndex :: Int -> SquareState
  reversePlayerIndex 1 = X
  reversePlayerIndex 0 = O
  reversePlayerIndex _ = error "ugh"

  winScore :: ProductGameState -> Int -> SquareState -> Int
  winScore board winID player = (winScores board)!(playerIndex player)!winID

  updateWinScores :: WinScoreArray -> QuickScoreArray -> SquareState -> [Int] ->
                     (WinScoreArray, QuickScoreArray)
  updateWinScores wins quick player [] = (wins, quick)
  updateWinScores wins quick player (winID:xs)
      | wins!p!winID == 8 = (wins, (quick // [(p, 10000), (opp, -10000)]))
      | otherwise         =
      updateWinScores (wins // [(p, wins!p // [(winID, 2 * wins!p!winID)]),
                                (opp, wins!opp // [(winID, 0)])])
                      (quick // [(p, quick!p + wins!p!winID),
                                 (opp, quick!opp - wins!opp!winID)]) player xs
      where p = playerIndex player
            opp = playerIndex (otherPlayer player)

  factorID :: FactorPair -> Int
  factorID (0, 0) = 0
  factorID (top, bottom) = (9 * (top - 1)) + (bottom - 1)

  squareUniqueID :: SquareXY -> SquareState -> Integer
  squareUniqueID coords player =
      mult * 3 ^ ((6 * row) + col + 4)
      where mult = fromIntegral (playerIndex (player) + 1)
            row = fromIntegral (fst coords)
            col = fromIntegral (snd coords)

  updateUniqueID :: ProductGameState -> SquareXY -> FactorPair ->
                    Integer
  updateUniqueID oldBoard (row, col) (top, bottom) =
      (uniqueIDPG oldBoard)
      + fromIntegral (factorID(top, bottom))
      + fromIntegral (squareUniqueID (row, col) (activePlayer oldBoard))
      - fromIntegral (factorID (topFactor oldBoard, bottomFactor oldBoard))
  
  reverseUniqueIDInner :: Integer ->
                          (SquareArray, FactorPair)
  reverseUniqueIDInner uniqueID
      | uniqueID <= 80 = (twoDArray height width SquareOpen, 
                          (fromIntegral(uniqueID `div` 9) + 1,
                           fromIntegral(uniqueID `mod` 9) + 1))
      | otherwise      = (nextSquares // [(row, nextSquares!row // [(col, reversePlayerIndex(thisDigit - 1))])], snd nextOne)
      where exp = floor (logBase 3 (fromIntegral(uniqueID)))
            thisDigit = fromIntegral(uniqueID `div` (3 ^ exp))
            row = (exp - 4) `div` 6
            col = (exp - 4) `mod` 6
            nextOne = reverseUniqueIDInner(uniqueID `mod` (3 ^ exp))
            nextSquares = fst nextOne

  guessMovesSoFar :: SquareArray -> Int
  guessMovesSoFar squares = length(filter (\x -> squares!(fst x)!(snd x) /= SquareOpen)  [(r,c) | r <- [0..(height - 1)], c <- [0..(width - 1)]])

  newProductGame :: ProductGameState
  newProductGame =
      ProductGameState { squares = (twoDArray height width SquareOpen)
                       , activePlayerPG = X
                       , topFactor = 0
                       , bottomFactor = 0
                       , movesSoFarPG = 0
                       , winScores = initialWinScores
                       , quickWinScores = (oneDArray 2 0)
                       , summaryPG = ""
                       , uniqueIDPG = 0
                       }

  newSquaresFromMove :: SquareArray -> FactorPair ->
                        SquareState -> SquareArray
  newSquaresFromMove squares (row, column) player =
      squares // [(row, squares!row // [(column, player)])] 

  newProductGameStateFromMove :: FactorPair -> ProductGameState ->
                                 ProductGameState
  newProductGameStateFromMove (top, bottom) oldBoard =
      ProductGameState { squares =
          newSquaresFromMove (squares oldBoard) coords (activePlayerPG oldBoard)
                       , activePlayerPG = otherPlayer (activePlayerPG oldBoard)
                       , topFactor = top
                       , bottomFactor = bottom
                       , movesSoFarPG = ((movesSoFarPG oldBoard) + 1)
                       , winScores = (fst scoreUpdate)
                       , quickWinScores = (snd scoreUpdate)
                       , summaryPG =
              (summaryPG oldBoard ++ " " ++ show (top, bottom))
                       , uniqueIDPG =
              updateUniqueID oldBoard coords (top, bottom)
                       }
      where coords = productCoords (top * bottom)
            scoreUpdate =
                updateWinScores (winScores oldBoard) (quickWinScores oldBoard)
                                (activePlayerPG oldBoard)
                                (winIDsForSquare coords)
    
  getHumanProductGameMove :: ProductGameState -> IO ProductGameState
  getHumanProductGameMove board = do
    putStrLn (show board)
    putStrLn "Top factor. If it's invalid in any way we will crash."
    top <- getLine
    putStrLn "Bottom factor. If it's invalid in any way we will crash."
    bottom <- getLine
    return (newProductGameStateFromMove (read top, read bottom) board)

  startGame = do
    playGame newProductGame

