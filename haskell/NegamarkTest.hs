import Negamark
import TicTacToe
import Test.HUnit
import Char
import Maybe

testBoard = foldr newTicTacToeStateFromMove newTicTacToeBoard [1,4,0,3]
winningBoard = foldr newTicTacToeStateFromMove newTicTacToeBoard [5,1,4,0,3]
xCanWinNow = foldr newTicTacToeStateFromMove newTicTacToeBoard [1,4,0,3]
xCouldLose = foldr newTicTacToeStateFromMove newTicTacToeBoard [6,7,4,8]
xLetOWin = foldr newTicTacToeStateFromMove newTicTacToeBoard [5,6,7,4,8]
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
    TestCase (assertEqual "13" (Outcome Loss 5 0)
              (firstPass winningBoard)),
    TestCase (assertEqual "14" (Outcome Loss 5 0)
              (fst (negamark winningBoard 0 (Outcome Loss 0 0) (Outcome Win 0 0)))),
    TestCase (assertEqual "15" (Outcome Heuristic 4 0)
              (firstPass testBoard)),
    TestCase (assertEqual "16" (Outcome Win 5 0)
              (fst (negamark testBoard 6 (Outcome Loss 0 0) (Outcome Win 0 0)))),
    TestCase (assertEqual "whatever" (Outcome Win 5 0)
              (fst (negamark testBoard 6 (Outcome Loss 0 0) (Outcome Win 0 0)))),
    TestCase (assertEqual "whatever" (Outcome Loss 7 0)
              (fst (negamark xIsGonnaWin 4 (Outcome Loss 0 0) (Outcome Win 0 0)))),
    TestCase (assertEqual "whatever" (Outcome Loss 9 0)
              (fst (negamark xWinsOnLastTurn 0 (Outcome Loss 0 0)(Outcome Win 0 0)))),
    TestCase (assertEqual "whatevz" [2, 5, 6, 7, 8] (availableMoves testBoard)),
    TestCase (assertEqual "nsimple on xcanwinnow" (Outcome Win 5 0)
              (fst (negamarkSimple xCanWinNow 5))),
    TestCase (assertEqual "whatever" X
              (squareState (head (snd (pickMove xCanWinNow 5))) 5)),
    TestCase (assertEqual "whatevz" 5 (length(allLegalMoves testBoard))),
    TestCase (assertEqual "X let O win" (Outcome Win 6 0)
              (fst (negamarkSimple xLetOWin 9))),
    TestCase (assertEqual "whatever" X
              (squareState (head (snd (pickMove xCouldLose 5))) 2)),
    TestCase (assertEqual "I leave this one at the bottom" 1 1)
    ]

main = runTestTT tests
