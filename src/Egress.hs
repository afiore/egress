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

type Command = String

readMigrations :: Options -> IO [Migration]
readMigrations opts = do
  let dir = migrationsDir opts
  fs <- getDirectoryContents dir
  return $ migrations $ (map ((</>) dir)) fs

printReport :: EgressState -> IO ()
printReport = mapM_ (putStrLn . show . messages)

handleCmd :: Command -> Maybe Int -> EgressState -> IO ()
handleCmd "version"  _       s = evalStateT readSchemaVersion s >>= putStrLn . show
handleCmd "rollback" Nothing s = execStateT 
handleCmd _ _ _                = usage

main :: IO ()
main = do
  args <- getArgs
  let (actions, cmds, errors) = getOpt Permute options args
  opts <- foldl (>>=) (return defaultOptions) actions

  let mVersion = version opts
  migs <- readMigrations opts
  conn <- connect $ dbConnection opts 

  case conn of
    (Left _)       -> (putStderr "Cannot connect to the database.") >> die
    (Right dbconn) -> do
      let s = EgressState dbconn [] migs

      case (cmds, errors) of
        ([], _:_)  -> mapM_ putStderr errors
        (cmd:_, _) -> handleCmd' cmd
        _          -> handleCmd' "usage"
      where
        putStderr    = hPutStr stderr
        die          = exitWith $ ExitFailure 1
        handleCmd' c = handleCmd c mVersion s
