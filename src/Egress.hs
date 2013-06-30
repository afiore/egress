import System.Directory
import System.FilePath((</>))
import System.Environment
import System.Console.GetOpt

import Egress.TypeDefs
import Egress.Migration
import Egress.DB
import Egress.Options

import Control.Monad.State
import Control.Egress

readMigrations :: Options -> IO [Migration]
readMigrations opts = do
  let dir = migrationsDir opts
  fs <- getDirectoryContents dir
  return $ migrations $ (map ((</>) dir)) fs

main :: IO ()
main = do
  args <- getArgs
  let (actions, cmds, errors) = getOpt Permute options args
  opts <- foldl (>>=) (return defaultOptions) actions

  let mVersion = version opts
  migs <- readMigrations opts
  conn <- connect $ dbConnection opts

  case conn of
    (Left _)       -> putStrLn "Cannot connect to the database"
    (Right dbconn) -> do
      case (cmds, errors) of
        ([], _:_) -> mapM_ putStrLn errors
        (_ , _)   -> do
                     let s = EgressState dbconn [] migs
                     (_, s') <- runStateT (buildAndRunPlan mVersion cmds) s
                     mapM_ (putStrLn . show) $ messages s'
