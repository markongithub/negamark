module Main where
  import HSQLMySQLTranspositionTable
  import Negamark
  import ProductGame

  main = do
    conn <- connectionFromFlags
    isLoss <- proveIsLossIO (newProductGameStateFromMoveSet newProductGame [(4,7)]) 18 (HSQLMySQLTranspositionTable conn 8)
    putStrLn (show isLoss)
