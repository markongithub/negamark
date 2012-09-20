--conn <- connect "localhost" "productgame" "productgame" ""
--stmt <- query conn "SELECT * FROM transposition WHERE state=8417859321065803640"
--fetch stmt
--getFieldValue stmt "value"

module HSQLMySQLTranspositionTableModule where
  import Control.Exception
  import Database.HSQL.MySQL
  import Negamark

  instance TranspositionTable HSQLMySQLTranspositionTable where
    getOutcome table state = getOutcomeMySQL table state
    saveOutcome table state outcome = saveOutcomeMySQL table state outcome
    maxMove table          = maxMoveMySQL table

  data HSQLMySQLTranspositionTable = HSQLMySQLTranspositionTable
      { connMySQL              :: Connection
      , maxMoveMySQL           :: Int
      }

  codeFromValue :: OutcomeValue -> Int
  codeFromValue value
      | value == Loss      = 20
      | value == Stalemate = 21
      | value == Heuristic = 22
      | value == Win       = 23
      | otherwise  = error "ugh"

  valueFromCode :: Int -> OutcomeValue
  valueFromCode code
      | code == 20 = Loss
      | code == 21 = Stalemate
      | code == 22 = Heuristic
      | code == 23 = Win
      | otherwise  = error "ugh"

  getOutcomeMySQL :: HSQLMySQLTranspositionTable -> Integer -> IO (Maybe Outcome)
  getOutcomeMySQL table state = do
      let conn = connMySQL table
      let query_text = "SELECT * FROM transposition WHERE state = " ++ show(state)
--      putStrLn query_text
      stmt <- query conn query_text
      fetched <- fetch stmt
      closeStatement stmt
      if fetched
        then do
          valueCode <- getFieldValue stmt "value"
          depth <- getFieldValue stmt "depth"
          heuristic <- getFieldValue' stmt "heuristic" 0
          return (Just (Outcome (valueFromCode valueCode) depth heuristic))
        else return Nothing

  insertQueryText :: Integer -> Outcome -> SQL
  insertQueryText state outcome =
     "REPLACE INTO transposition (state, value, depth, heuristic) VALUES (" ++
     show(state) ++ ", " ++ show(codeFromValue(value outcome)) ++ ", " ++ 
     show(depth outcome) ++ ", " ++ show(heuristic outcome) ++ ")"
 
  saveOutcomeMySQL :: HSQLMySQLTranspositionTable -> Integer -> Outcome -> IO()
  saveOutcomeMySQL table state outcome = do
      let conn = connMySQL table
      let query_text = insertQueryText state outcome
      putStrLn query_text
      stmt <- query conn query_text
      closeStatement stmt

--  myConn <- connect "localhost" "productgame" "productgame" ""
--  myTable = HSQLMySQLTranspositionTable myConn 8


