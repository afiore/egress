import System.Directory
import System.FilePath((</>))
import System.Environment
import System.Console.GetOpt
import System.IO
import System.Exit

import Egress.TypeDefs
import Egress.Migration
import Egress.DB
import Egress.Options

import Control.Monad.State
import Control.Egress

handleCmd :: String -> Maybe Int -> EgressState -> IO ()
handleCmd "version"     _        s = evalStateT readSchemaVersion s >>= putStrLn . show
handleCmd "set-version" (Just v) s = execStateT (setVersion v) s >>= printReport
handleCmd "up"          Nothing  s = execStateT runUpgradePlan  s >>= printReport
handleCmd "rollback"    Nothing  s = execStateT runRollbackPlan s >>= printReport
handleCmd _             _        _ = usage

readMigrations :: Options -> IO [Migration]
readMigrations opts = do
  let dir = migrationsDir opts
  fs <- getDirectoryContents dir
  return $ migrations $ (map ((</>) dir)) fs

printReport :: EgressState -> IO ()
printReport s = mapM_ (putStrLn . show) $ messages s

main :: IO ()
main = do
  args <- getArgs
  let (actions, cmds, errors) = getOpt Permute options args
  opts <- foldl (>>=) (return defaultOptions) actions

  let mVersion  = version opts

  migs <- readMigrations opts
  conn <- connect $ dbConnection opts 

  case conn of
    (Left _)       -> die "Cannot connect to the database."
    (Right dbconn) -> do
      let s = EgressState dbconn [] migs
          handleCmd' c = handleCmd c mVersion s

      case (cmds, errors) of
        ([], _:_)  -> mapM_ putStderr errors
        (cmd:_, _) -> handleCmd' cmd
        _          -> handleCmd' "usage"
  where
    putStderr = hPutStr stderr
    die msg   = putStderr msg >> exitWith (ExitFailure 1)
