module Egress.DB (
  readSchemaVersion
  , writeSchemaVersion
  , runMigration
  , runPlan
  , connect
  , Egress
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Egress.TypeDefs
import Control.Monad.State
import Control.Exception

type Egress a  = StateT (EgressState Connection) IO a
type SafeSql a = Either SqlError a

schemaTable      :: String
schemaTable      = "schema_version"

sqlCreateTable   :: String
sqlCreateTable   = "CREATE TABLE " ++ schemaTable ++ " (version INTEGER NOT NULL UNIQUE)"

sqlInsertVersion :: String
sqlInsertVersion = "INSERT INTO " ++ schemaTable ++ " (version) VALUES (0)"

sqlSelectVersion :: String
sqlSelectVersion = "SELECT version FROM " ++ schemaTable ++ " LIMIT 1"

sqlUpdateVersion :: String
sqlUpdateVersion = "UPDATE "++ schemaTable ++ " SET version = ?"

connect :: FilePath -> IO (SafeSql Connection)
connect fp = do
  dbh <- try $ connectSqlite3 fp
  prepDB dbh

prepDB :: IConnection conn => SafeSql conn -> IO (SafeSql conn)
prepDB err@(Left _) = return err
prepDB (Right dbh) = do
  tables <- getTables dbh
  when (not (schemaTable `elem` tables)) $ do
    forM_ [sqlCreateTable, sqlInsertVersion] $ \q -> do
      run dbh q []
  return $ Right dbh

getConnection :: Egress Connection
getConnection = do
  s <- get
  return $ connection s

infoMsg :: String -> Egress ()
infoMsg msg = do
  modify (\ s -> s { messages = (messages s) ++ [CliInfoMsg msg] })

runQueryM :: Maybe Migration -> String -> [SqlValue] -> Egress [[SqlValue]]
runQueryM maybeMigr q vs = do
  conn          <- getConnection
  eitherResults <- liftIO $ try $ quickQuery conn q vs

  case eitherResults of
    Left err      -> (errorMsg maybeMigr err) >> return []
    Right results -> return results

runQuery :: String -> [SqlValue] -> Egress [[SqlValue]]
runQuery = runQueryM Nothing

errorMsg :: Maybe Migration -> SqlError -> Egress ()
errorMsg Nothing  err = do
  modify (\ s -> s { messages = (messages s) ++ [CliInternalErrorMsg $ seErrorMsg err] })
errorMsg (Just _) err = do
  modify (\ s -> s { messages = (messages s) ++ [CliErrorMsg $ seErrorMsg err] })

readSchemaVersion :: Egress Int
readSchemaVersion = do
  results <- runQuery sqlSelectVersion []
  return $ case results of
    (version:_):_ -> fromSql version
    _             -> 0

commitDb :: Egress ()
commitDb = do
  conn <- getConnection
  liftIO $ commit conn

writeSchemaVersion :: Int -> Egress ()
writeSchemaVersion v = do
  (runQuery sqlUpdateVersion $ [toSql v]) >> return ()

runMigration :: Migration -> Egress ()
runMigration m@(Migration version _ mpath)= do
  q <- liftIO $ do
    readFile mpath
  let msg = "Executing migration:" ++ mpath
  (infoMsg msg) >> (runQueryM (Just m) q []) >> writeSchemaVersion version

runPlan :: Int -> [Migration] -> Egress ()
runPlan v plan = do
  _ <- mapM_ runMigration plan
  (writeSchemaVersion v) >> commitDb
