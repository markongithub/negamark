module Main where
  import Database.HSQL.MySQL
  import HSQLMySQLTranspositionTable
  import Negamark
  import ProductGame

  main = do
    conn <- connectionFromFlags
    let table = HSQLMySQLTranspositionTable conn 8
    playGameIO modifiedProductGame True False 10 table
