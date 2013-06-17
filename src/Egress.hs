import Data.Maybe(fromMaybe)
import System.Directory
import System.FilePath((</>))
import System.Environment
import System.Console.GetOpt
import Egress.TypeDefs
import Egress.Migration
import Egress.DB
import Egress.Options

type Command = [String]

buildAndRunPlan :: [Migration] -> Options -> Command -> IO ()
buildAndRunPlan [] _ _ = do
  print "No migrations found"

buildAndRunPlan migs opts cmd = do
  db <- connect $ dbConnection opts
  sv <- readSchemaVersion db

  let from  = fromMaybe 0 sv
  let to    = case (version opts, cmd) of
                (Just v', _)            -> v'
                (Nothing, "rollback":_) -> previousVersion from migs
                _                       -> mId $ last migs

  runPlan db to $ migrationPlan (Range from to) migs

readMigrations :: Options -> IO [Migration]
readMigrations opts = do
  let dir = migrationsDir opts
  fs <- getDirectoryContents dir
  return $ migrations $ (map ((</>) dir)) fs

main :: IO ()
main = do
  args  <- getArgs
  let (actions, cmds, errors) = getOpt Permute options args
  opts  <- foldl (>>=) (return defaultOptions) actions
  migs  <- readMigrations opts

  case (cmds, errors) of
    ([], _:_) -> print "an error occured"
    (_ , _)   -> buildAndRunPlan migs opts cmds
