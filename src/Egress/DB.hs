module Egress.DB (
  connect
  , sqlCreateTable
  , sqlInsertVersion
  , sqlSelectVersion
  , sqlUpdateVersion
  , SafeSql
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad.State
import Control.Exception

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
prepDB e@(Left _) = return e
prepDB (Right conn) = do
  tables <- getTables conn
  let schemaMissing = not $ schemaTable `elem` tables
  when schemaMissing $ do
    mapM_ (executeQuery conn) [sqlCreateTable, sqlInsertVersion]
  return $ Right conn

executeQuery :: IConnection conn => conn -> String -> IO Integer
executeQuery dbh q = run dbh q []
