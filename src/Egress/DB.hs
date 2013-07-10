module Egress.DB (
  sqlCreateTable
  , sqlInsertVersion
  , sqlSelectVersion
  , sqlUpdateVersion
  , connect
  , SafeSql
  , DbAdapter(..)
  , DbAdapterName(..)
) where

import           Data.List (isPrefixOf)

import           Database.HDBC
import qualified Database.HDBC.Sqlite3    as Sqlite
import qualified Database.HDBC.PostgreSQL as Postgres

import           Control.Monad
import           Control.Exception

type SafeSql a = Either SqlError a

data DbAdapterName    = DbSqlite | DbPostgres deriving (Show, Eq)

instance Read DbAdapterName where
  readsPrec _ s | "sqlite"   `isPrefixOf` s = [(DbSqlite, drop 5 s)]
                | "postgres" `isPrefixOf` s = [(DbPostgres, drop 8 s)]
                | otherwise                 = []

data DbAdapter = SqliteAdapter   Sqlite.Connection
               | PostgresAdapter Postgres.Connection

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

connect :: String -> FilePath -> IO (SafeSql DbAdapter)
connect "postgres" dbPath = do
  dbh  <- try $ PostgresAdapter `liftM` (Postgres.connectPostgreSQL dbPath)
  prepDB dbh
connect _ dbPath = do
  dbh  <- try $ SqliteAdapter `liftM` (Sqlite.connectSqlite3 dbPath)
  prepDB dbh

prepDB :: SafeSql DbAdapter -> IO (SafeSql DbAdapter)
prepDB e@(Left _) = return e
prepDB (Right (PostgresAdapter conn)) = do
  prepDB' conn >>= return . Right . PostgresAdapter
prepDB (Right (SqliteAdapter   conn)) = do
  prepDB' conn >>= return . Right . SqliteAdapter

prepDB' :: IConnection c => c -> IO c
prepDB' conn = do
  tables <- getTables conn
  let schemaMissing = not $ schemaTable `elem` tables
  when schemaMissing $ do
    mapM_ (executeQuery conn) [sqlCreateTable, sqlInsertVersion]
  return conn

executeQuery :: IConnection c => c -> String -> IO Integer
executeQuery dbh q = run dbh q []
