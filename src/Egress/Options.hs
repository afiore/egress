module Egress.Options where

import Data.Char (toLower)
import Data.List (isPrefixOf)

import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import Egress.DB (DbAdapterName(..))

data Options = Options {
  verbose         :: Bool
  , silent        :: Bool
  , version       :: Maybe Int
  , dbConnection  :: String
  , migrationsDir :: String
  , dbAdapter     :: DbAdapterName
} deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
  verbose         = False
  , silent        = False
  , version       = Nothing
  , dbConnection  = "example.sqlite3"
  , migrationsDir = "migrations"
  , dbAdapter     = DbSqlite
}

options :: [OptDescr (Options -> IO Options)]
options = [
    Option "v" ["schema-version"]
             (ReqArg (\arg opts -> return opts { version = Just (read arg :: Int) }) "n")
             "Target schema version"

    , Option "m" ["migration-dir"]
             (ReqArg (\arg opts -> return opts { migrationsDir = arg }) "./migrations-dir")
             "Path to the migrations folder"

    , Option "d" ["db-connection"]
             (ReqArg (\arg opts -> return opts { dbConnection = arg }) "./dbs/example.sqlite3")
             "DB connection string"

    , Option "D" ["driver"]
             (ReqArg (\arg opts -> handleAdapterOpt arg opts) "sqlite|postgres")
             "HDBC Adapter (i.e. sqlite, postgres)"

    , Option "V" ["verbose"]
             (NoArg (\opts -> return opts { verbose = True, silent = False }))
             "Verbose mode"

    , Option "s" ["silent"]
             (NoArg (\opts -> return opts { silent = True, verbose = False }))
             "Silent mode"

    , Option "h" ["help"]
             (NoArg (\_ -> usage >> exitWith ExitSuccess))
             "Show help"
  ]

handleAdapterOpt :: String -> Options -> IO Options
handleAdapterOpt arg opts 
  | map toLower arg `isPrefixOf` "postgres" = return opts { dbAdapter = DbPostgres }
  | map toLower arg `isPrefixOf` "sqlite3"  = return opts { dbAdapter = DbSqlite   }
  | otherwise = do
      hPutStr stderr "The 'driver' option -d must be set to either 'postgres' or 'sqlite'"
      exitFailure

usage :: IO ()
usage = do
  prg <- getProgName
  let header = "Usage: " ++ prg ++ " [options] [version|up|rollback]\n\n" ++
               "Options:"
  hPutStr stderr $ usageInfo header options
