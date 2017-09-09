module Main where
  import Database.HSQL.MySQL
  import HSQLMySQLTranspositionTable
  import Negamark
  import ProductGame

  main = do
    conn <- connect "localhost" "productgame" "productgame" ""
    isLoss <- proveIsLossIO (newProductGameStateFromMoveSet newProductGame [(4,7)]) 18 (HSQLMySQLTranspositionTable conn 8)
    putStrLn (show isLoss)
