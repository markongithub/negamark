import Negamark
import Test.HUnit
import Char
import Maybe

testBoard = foldr newTicTacToeStateFromMove newTicTacToeBoard [1,4,0,3]
winningBoard = foldr newTicTacToeStateFromMove newTicTacToeBoard [5,1,4,0,3]
xIsGonnaWin = foldr newTicTacToeStateFromMove newTicTacToeBoard [2,1,5,0,4]
xWinsOnLastTurn =
    foldr newTicTacToeStateFromMove newTicTacToeBoard [6,8,7,3,2,1,5,0,4]

tests = TestList [
    TestCase (assertEqual "whatever" 19 (depth(Outcome Loss 19 0))),
    TestCase (assertEqual "whatever" 
              (Outcome Heuristic 19 (-22))
              (opposite (Outcome Heuristic 19 22))),
    TestCase (assertBool "whatever" 
              ((Outcome Stalemate 36 0) > (Outcome Loss 19 0))),
    TestCase (assertBool "whatever" 
              ((Outcome Loss 36 0) > (Outcome Loss 19 0))),
    TestCase (assertBool "whatever" 
              ((Outcome Win 19 0) > (Outcome Win 36 0))),
    TestCase (assertEqual "whatever" (Outcome Heuristic 19 (-22))
              (opposite (Outcome Heuristic 19 22))),
    TestCase (assertEqual "whatever" SquareOpen
              (squareState newTicTacToeBoard 2)),
    TestCase (assertEqual "whatever" SquareOpen
              (valueFromSquares newTicTacToeBoard (3, 4, 5))),
    TestCase (assertEqual "whatever" X
              (squareState (newTicTacToeStateFromMove 0 newTicTacToeBoard) 0)),
    TestCase (assertEqual "whatever" X
              (valueFromSquares winningBoard (3, 4, 5))),
    TestCase (assertEqual "whatever" X
              (findWinner winningBoard waysToWin)),
    TestCase (assertEqual "whatever" (Outcome Loss 5 0)
              (firstPass winningBoard)),
    TestCase (assertEqual "whatever" (Outcome Loss 5 0)
              (negamark winningBoard 0 (Outcome Loss 0 0) (Outcome Win 0 0))),
    TestCase (assertEqual "whatever" (Outcome Heuristic 4 0)
              (firstPass testBoard)),
    TestCase (assertEqual "whatever" (Outcome Win 5 0)
              (negamark testBoard 6 (Outcome Loss 0 0) (Outcome Win 0 0))),
    TestCase (assertEqual "whatever" (Outcome Loss 7 0)
              (negamark xIsGonnaWin 4 (Outcome Loss 0 0) (Outcome Win 0 0))),
    TestCase (assertEqual "whatever" (Outcome Loss 9 0)
              (negamark xWinsOnLastTurn 0 (Outcome Loss 0 0) (Outcome Win 0 0))),
    TestCase (assertEqual "whatevz" [2, 5, 6, 7, 8] (availableMoves testBoard)),
    TestCase (assertEqual "I leave this one at the bottom" 1 1)
    ]

main = runTestTT tests
