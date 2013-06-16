module Egress.Options where

import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

data Options = Options {
  verbose         :: Bool
  , silent        :: Bool
  , version       :: Maybe Int
  , dbConnection  :: String
  , migrationsDir :: String
} deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
  verbose         = False
  , silent        = False
  , version       = Nothing
  , dbConnection  = "example.sqlite3"
  , migrationsDir = "migrations"
}

options :: [OptDescr (Options -> IO Options)]
options = [
    Option "v" ["schema-version"]
             (ReqArg (\arg opts -> return opts { version = Just (read arg :: Int) }) "n")
             "Target schema version"

    , Option "m" ["migration-dir"]
             (ReqArg (\arg opts -> return opts { dbConnection = arg }) "./migrations-dir")
             "Path to the migrations folder"

    , Option "d" ["db-connection"]
             (ReqArg (\arg opts -> return opts { migrationsDir = arg }) "./dbs/example.sqlite3")
             "DB connection string"

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

usage :: IO ()
usage = do
  prg <- getProgName
  let header = "Usage: " ++ prg ++ " [options] [up|rollback]\n\n" ++
               "If no command is specified 'up' is implicitly assumed.\n\n" ++
               "Options:"
  hPutStr stderr $ usageInfo header options
