import Negamark
import System.Exit
import TicTacToe
import Test.HUnit
import Data.Maybe

testBoard = foldr newTicTacToeStateFromMove newTicTacToeBoard [1,4,0,3]
winningBoard = foldr newTicTacToeStateFromMove newTicTacToeBoard [5,1,4,0,3]
xCanWinNow = foldr newTicTacToeStateFromMove newTicTacToeBoard [1,4,0,3]
xCouldLose = foldr newTicTacToeStateFromMove newTicTacToeBoard [6,7,4,8]
xLetOWin = foldr newTicTacToeStateFromMove newTicTacToeBoard [5,6,7,4,8]
xIsGonnaWin = foldr newTicTacToeStateFromMove newTicTacToeBoard [2,1,5,0,4]
xWinsOnLastTurn =
    foldr newTicTacToeStateFromMove newTicTacToeBoard [6,8,7,3,2,1,5,0,4]
negamark3Bug = foldr newTicTacToeStateFromMove newTicTacToeBoard $ reverse [0,4,1,2]

tests = TestList [
    TestCase (assertEqual "whatever" 
              (Heuristic 19 (-22))
              (opposite (Heuristic 19 22))),
    TestCase (assertBool "whatever" 
              ((Stalemate 36) > (Loss 19))),
    TestCase (assertBool "whatever" 
              ((Loss 36) > (Loss 19))),
    TestCase (assertBool "whatever" 
              ((Win 19) > (Win 36))),
    TestCase (assertEqual "whatever" (Heuristic 19 (-22))
              (opposite (Heuristic 19 22))),
    TestCase (assertEqual "whatever" SquareOpen
              (squareState newTicTacToeBoard 2)),
    TestCase (assertEqual "8" SquareOpen
              (valueFromSquares newTicTacToeBoard (3, 4, 5))),
    TestCase (assertEqual "whatever" X
              (squareState (newTicTacToeStateFromMove 0 newTicTacToeBoard) 0)),
    TestCase (assertEqual "whatever" X
              (valueFromSquares winningBoard (3, 4, 5))),
    TestCase (assertEqual "whatever" X
              (findWinner winningBoard)),
    TestCase (assertEqual "whatever" X
              (findTicTacToeWinner winningBoard waysToWin)),
    TestCase (assertEqual "13" (Loss 5)
              (firstPass winningBoard)),
    TestCase (assertEqual "14" (Loss 5)
              (resultOutcome (negamark winningBoard 0 (Loss 0) (Win 0)))),
    TestCase (assertEqual "14.3" (Loss 5)
              (resultOutcome (negamark3 winningBoard 0 (Loss 0) (Win 0) noMap))),
    TestCase (assertEqual "15" (Heuristic 4 0)
              (firstPass testBoard)),
    TestCase (assertEqual "16" (Win 5)
              (resultOutcome (negamark testBoard 6 (Loss 0) (Win 0)))),
    TestCase (assertEqual "whatever" (Win 5)
              (resultOutcome (negamark testBoard 6 (Loss 0) (Win 0)))),
    TestCase (assertEqual "whatever" (Loss 7)
              (resultOutcome (negamark xIsGonnaWin 4 (Loss 0) (Win 0)))),
    TestCase (assertEqual "whatever" (Loss 9)
              (resultOutcome (negamark xWinsOnLastTurn 0 (Loss 0)(Win 0)))),
    TestCase (assertEqual "16" (Win 5)
              (resultOutcome (negamark3 testBoard 6 (Loss 0) (Win 0) noMap))),
    TestCase (assertEqual "whatever" (Win 5)
              (resultOutcome (negamark3 testBoard 6 (Loss 0) (Win 0) noMap))),
    TestCase (assertEqual "whatever" (Loss 7)
              (resultOutcome (negamark3 xIsGonnaWin 4 (Loss 0) (Win 0) noMap))),
    TestCase (assertEqual "whatever" (Loss 9)
              (resultOutcome (negamark3 xWinsOnLastTurn 0 (Loss 0)(Win 0) noMap))),
    TestCase (assertEqual "whatevz" [2, 5, 6, 7, 8] (availableMoves testBoard)),
    TestCase (assertEqual "nsimple on xcanwinnow" (Win 5)
              (resultOutcome (negamarkSimple xCanWinNow 5))),
    TestCase (assertEqual "whatever" X
              (squareState (head (resultMoves (pickMove xCanWinNow 5))) 5)),
    TestCase (assertEqual "whatevz" 5 (length(allLegalMoves testBoard))),
    TestCase (assertEqual "X let O win" (Win 6)
              (resultOutcome (negamarkSimple xLetOWin 9))),
    TestCase (assertEqual "whatever" X
              (squareState (head (resultMoves (pickMove xCouldLose 5))) 2)),
    TestCase (assertEqual "the first negamark3 bug" (Stalemate 9)
              (resultOutcome (negamarkSimple negamark3Bug 6))),
    TestCase (assertEqual "the first negamark3 bug" (Stalemate 9)
              (resultOutcome (negamarkSimple3 negamark3Bug 6))),
    TestCase (assertEqual "slow negamark on TTT" (Stalemate 9)
              (resultOutcome (negamark newTicTacToeBoard 9 (Loss 0) (Win 0)))),
    TestCase (assertEqual "slow negamark3 on TTT" (Stalemate 9)
              (resultOutcome (negamark3 newTicTacToeBoard 9 (Loss 0) (Win 0) noMap))),
    TestCase (assertEqual "I leave this one at the bottom" 1 1)
    ]

main = do
  cs@(Counts _ _ errs fails) <- runTestTT tests
  putStrLn (showCounts cs)
  if (errs > 0 || fails > 0)
    then exitFailure
    else exitSuccess
