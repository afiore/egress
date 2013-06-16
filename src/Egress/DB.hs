module Egress.DB (
    readSchemaVersion
  , writeSchemaVersion
  , runMigration
  , runPlan
  , connect
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Egress.TypeDefs
import Control.Monad(when,forM)

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

connect :: FilePath -> IO Connection
connect fp = do
  dbh <- connectSqlite3 fp
  prepDB dbh
  return dbh

prepDB :: IConnection conn => conn -> IO ()
prepDB dbh = do
  tables <- getTables dbh
  when (not (schemaTable `elem` tables)) $ do
    _ <- forM [sqlCreateTable, sqlInsertVersion] $ \q -> do
      run dbh q []
    return ()

readSchemaVersion :: IConnection conn => conn -> IO (Maybe Int)
readSchemaVersion dbh = do
  stmt <- prepare dbh sqlSelectVersion
  _    <- execute stmt []
  row  <- fetchRow stmt
  return $ case row of
                Just (version:_) -> Just $ fromSql version
                _                -> Nothing

writeSchemaVersion :: IConnection conn => conn -> Int -> IO ()
writeSchemaVersion dbh version = do
  _ <- run dbh sqlUpdateVersion [toSql version]
  _ <- commit dbh
  return ()

runMigration :: IConnection conn => conn -> Migration -> IO ()
runMigration dbh (Migration version _ mpath)= do
  query <-readFile mpath
  _ <- run dbh query []
  _ <- run dbh sqlUpdateVersion [toSql version]
  _ <- commit dbh
  return ()

runPlan :: IConnection conn => conn -> Int -> [Migration] -> IO ()
runPlan db to plan = do
  _ <- mapM (runMigration db) plan
  _ <- writeSchemaVersion db to
  return ()
