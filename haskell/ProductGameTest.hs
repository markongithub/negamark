import Negamark
import ProductGame
import Test.HUnit
import Char
import Maybe

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

tests = TestList [
    TestCase (assertEqual "whatever" 19 (depth(Outcome Loss 19 0)))
  , TestCase (assertEqual "whatever" 
              (Outcome Heuristic 19 (-22))
              (opposite (Outcome Heuristic 19 22)))
  , TestCase (assertEqual "" X (squareState fourSquare 2 1))
  , TestCase (assertEqual "" SquareOpen (squareState fourSquare 2 2))
  , TestCase (assertEqual "" 45 (length (availableMoves newProductGame)))
  , TestCase (assertEqual "" 8 (length (availableMoves fourSquare)))
  , TestCase (assertEqual "" [(0,0), (0,1), (0,2), (0,3)]
                          (allIndicesForTetrad goRight (0,0)))
  , TestCase (assertEqual "" 54 (length allWinningTetrads))
  , TestCase (assertEqual "" X (findWinner xHasWon))
  , TestCase (assertEqual "" (Outcome Loss 7 0)
                          (fst (negamarkSimple xHasWon 0)))
    ]

main = runTestTT tests
