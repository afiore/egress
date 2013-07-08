module Egress.DB (
  connectPostgres
  , connectSqlite
  , sqlCreateTable
  , sqlInsertVersion
  , sqlSelectVersion
  , sqlUpdateVersion
  , SafeSql
  , DbAdapter(..)
) where

import           Database.HDBC
import qualified Database.HDBC.Sqlite3    as Sqlite
import qualified Database.HDBC.PostgreSQL as Postgres

import           Control.Monad.State
import           Control.Exception

type SafeSql a = Either SqlError a

data DbAdapter = Sqlite3
               | Postgres
  deriving (Show, Read, Eq)


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

connectPostgres :: FilePath -> IO (SafeSql Postgres.Connection)
connectPostgres fp = do
  dbh <- try $ Postgres.connectPostgreSQL fp
  prepDB dbh

connectSqlite :: FilePath -> IO (SafeSql Sqlite.Connection)
connectSqlite fp = do
  dbh <- try $ Sqlite.connectSqlite3 fp
  prepDB dbh

prepDB :: IConnection c => SafeSql c -> IO (SafeSql c)
prepDB e@(Left _) = return e
prepDB (Right conn) = do
  tables <- getTables conn
  let schemaMissing = not $ schemaTable `elem` tables
  when schemaMissing $ do
    mapM_ (executeQuery conn) [sqlCreateTable, sqlInsertVersion]
  return $ Right conn

executeQuery :: IConnection c => c -> String -> IO Integer
executeQuery dbh q = run dbh q []
