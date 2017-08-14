module Main where
  import Database.HSQL.MySQL
  import HSQLMySQLTranspositionTableModule
  import Negamark
  import ProductGame

  main = do
    conn <- connect "localhost" "productgame" "productgame" "passwordsarestupid"
    let table = HSQLMySQLTranspositionTable conn 8
        proofs = [
          proveIsLossIO (newProductGameStateFromMoveSet newProductGame [(4,7)]) 18 table
          , proveIsLossIO (newProductGameStateFromMoveSet newProductGame [(3,6)]) 18 table
          , proveIsLossIO (newProductGameStateFromMoveSet newProductGame [(4,5)]) 20 table
          , proveIsLossIO (newProductGameStateFromMoveSet newProductGame [(1,9),(2,9),(2,5),(2,6),(2,8)]) 20 table -- @25
          , proveIsWinIO (newProductGameStateFromMoveSet newProductGame [(1,9),(2,9),(2,5),(2,4)]) 21 table -- @25
          , proveIsLossIO (newProductGameStateFromMoveSet newProductGame [(1,9),(2,9),(2,5)]) 18 table -- @21
          , proveIsWinIO (newProductGameStateFromMoveSet newProductGame [(1,9),(1,8),(4,8),(4,5)]) 21 table -- @21
          , proveIsWinIO (newProductGameStateFromMoveSet newProductGame [(1,9),(1,8)]) 19 table -- @21
          , proveIsLossIO (newProductGameStateFromMoveSet newProductGame [(1,9)]) 20 table -- @21
          , proveIsLossIO (newProductGameStateFromMoveSet newProductGame [(2,8)]) 20 table -- @21
          , proveIsLossIO modifiedProductGame 3 table
            ]
    outcomes <- sequence proofs
    putStrLn (show outcomes)
