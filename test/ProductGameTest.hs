import Negamark
import ProductGame
import Test.HUnit
import Data.Maybe

fourSquare = foldr newProductGameStateFromMove newProductGame [(4,4)]
xCanWin = foldr newProductGameStateFromMove newProductGame [
    (4,3), (4,5), (4,9), (2,9), (2,4), (4,4)]
xHasWon = foldr newProductGameStateFromMove newProductGame [
    (7,3), (4,3), (4,5), (4,9), (2,9), (2,4), (4,4)]
buggyCase = foldr newProductGameStateFromMove newProductGame (reverse [
    (4,4), (2,4), (2,9), (1,9), (8,9), (3,9), (3,7), (1,7), (4,7), (6,7),
    (7,7), (7,8), (7,9), (7,2), (2,5), (2,3), (2,6)])
principalVariation = reverse [
    (4,7), (1,7), (3,7), (1,3), (3,6), (1,6), (5,6), (1,5), (1,4), (4,5),
    (2,4), (4,9), (3,9), (3,3), (3,8), (4,8)]
afterTwoMoves = foldr newProductGameStateFromMove newProductGame (reverse [
    (1,1), (1,7)])
allFactorPairs = [(x, y) | x <- [0..9], y <- [0..9], x<=y]

tests = TestList [
    TestCase (assertEqual "whatever" 
              (Heuristic 19 (-22))
              (opposite (Heuristic 19 22)))
  , TestCase (assertEqual "" X (squareState (squares fourSquare) 2 1))
  , TestCase (assertEqual "" SquareOpen (squareState (squares fourSquare) 2 2))
  , TestCase (assertEqual "" 45 (length (availableMoves newProductGame)))
  , TestCase (assertEqual "" 8 (length (availableMoves fourSquare)))
  , TestCase (assertEqual "" [(0,0), (0,1), (0,2), (0,3)]
                          (allIndicesForTetrad goRight (0,0)))
  , TestCase (assertEqual "" 54 (length allWinningTetrads))
  , TestCase (assertEqual "" X (findWinner xHasWon))
  , TestCase (assertEqual "" (Loss 7)
                          (resultOutcome (negamarkSimple xHasWon 0)))
  , TestCase (assertEqual "" 0  (factorID (1,1)))
  , TestCase (assertEqual "" 80 (factorID (9,9)))
  , TestCase (assertEqual "" (2 * 81) -- 81 + 0 + 0
                          (uniqueID (newProductGameStateFromMove (1,1)
                                     newProductGame)))
-- (2 * (3^10)) + (1 * 3 ^ 4) + (1-1) + (7-1)
  , TestCase (assertEqual "2 9" ((2 * (3^(14+4))) + (9 * (2-1)) + (9-1))
                          (uniqueID (newProductGameStateFromMove (2,9)
                                     newProductGame)))
-- (1 * (3^(20+4)))
  , TestCase (assertEqual "squareun" 282429536481 (squareUniqueID (3,2) O))
  , TestCase (assertEqual "" ((2 * (3^(19+4))) + (9 * (3-1)) + (9-1))
                          (uniqueID (newProductGameStateFromMove (3,9)
                                     newProductGame)))
  , TestCase (assertEqual "" allFactorPairs (map (reverseTopBottomID . fromIntegral . factorID) allFactorPairs))
    ]

main = runTestTT tests
