--conn <- connect "localhost" "productgame" "productgame" ""
--stmt <- query conn "SELECT * FROM transposition WHERE state=8417859321065803640"
--fetch stmt
--getFieldValue stmt "value"

module HSQLMySQLTranspositionTable where
  import Control.Exception
  import Data.Data (Data) -- Data
  import Database.HSQL.MySQL
  import Negamark hiding (summary)
  import System.Console.CmdArgs.Implicit--  (cmdArgs, def, explicit, name, opt, help, summary, typFile, (&=))

  type SQLQuery = String

  instance TranspositionTable HSQLMySQLTranspositionTable where
    getOutcome table state = getOutcomeMySQL table state
    saveOutcome table state outcome = saveOutcomeMySQL table state outcome
    maxMove table          = maxMoveMySQL table

  data HSQLMySQLTranspositionTable = HSQLMySQLTranspositionTable
      { connMySQL              :: Connection
      , maxMoveMySQL           :: Int
      }

  data MySQLConnectionFlags = MySQLConnectionFlags {
      mysql_address :: String
    , mysql_database :: String
    , mysql_username :: String
    , mysql_password :: String } deriving (Data, Show)

  connectionFromFlags :: IO Connection
  connectionFromFlags = do
    let caVal = MySQLConnectionFlags
                  ("" &= name "mysql_address" &= help "address of the mysql server" &= explicit)
                  ("productgame" &= name "mysql_database" &= help "database in which table is stored" &= explicit) 
                  ("productgame" &= name "mysql_username" &= help "username with which to authenticate" &= explicit)
                  ("" &= name "mysql_password" &= help "password with which to authenticate" &= explicit)
    flags <- cmdArgs caVal
    let (MySQLConnectionFlags addr db username password) = flags
    connect addr db username password

  outcomeFromRecord :: String -> MoveNumber -> HeuristicValue -> Outcome
  outcomeFromRecord value depth heuristic
      | value == "Loss"      = Loss depth
      | value == "Stalemate" = Stalemate depth
      | value == "Heuristic" = Heuristic depth heuristic
      | value == "Win"       = Win depth
      | otherwise  = error ("We got an invalid value: " ++ value)

  getOutcomeMySQL :: HSQLMySQLTranspositionTable -> Integer -> IO (Maybe Outcome)
  getOutcomeMySQL table state = do
      let conn = connMySQL table
      let query_text = "SELECT * FROM transposition WHERE state = " ++ show(state)
--      putStrLn query_text
      stmt <- query conn query_text
      fetched <- fetch stmt
      if fetched
        then do
          value <- getFieldValue stmt "value"
          depth <- getFieldValue stmt "depth"
          heuristic <- getFieldValue' stmt "heuristic" 0
          closeStatement stmt
          return (Just (outcomeFromRecord value depth heuristic))
        else do
          closeStatement stmt
          return Nothing

  sqlValues :: Outcome -> (String, String, String)
  sqlValues (Loss depth)                = ("'Loss'", show depth, "NULL")
  sqlValues (Stalemate depth)           = ("'Stalemate'", show depth, "NULL")
  sqlValues (Heuristic depth heuristic) = ("'Heuristic'", show depth,
                                           show heuristic)
  sqlValues (Win depth)                 = ("'Win'", show depth, "NULL")

  insertQueryText :: Integer -> Outcome -> SQLQuery
  insertQueryText state outcome =
      "REPLACE INTO transposition (state, value, depth, heuristic) VALUES (" ++
      show(state) ++ ", " ++ valueCode ++ ", " ++ depth ++ ", " ++ heuristic ++ 
      ")"
    where (valueCode, depth, heuristic) = sqlValues outcome 
 
  saveOutcomeMySQL :: HSQLMySQLTranspositionTable -> Integer -> Outcome -> IO()
  saveOutcomeMySQL table state outcome = do
      let conn = connMySQL table
      let query_text = insertQueryText state outcome
--      putStrLn query_text
      stmt <- query conn query_text
      closeStatement stmt

--  myConn <- connect "localhost" "productgame" "productgame" ""
--  myTable = HSQLMySQLTranspositionTable myConn 8


