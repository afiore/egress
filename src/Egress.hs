import System.Directory
import System.FilePath((</>))
import System.Environment
import System.Console.GetOpt
import Egress.TypeDefs
import Egress.Migration
import Egress.DB
import Egress.Options
import Control.Monad.Reader


type Command = [String]

buildAndRunPlan :: [Migration] -> Maybe Int -> Command -> Egress ()
buildAndRunPlan [] _ _ = liftIO $ putStrLn "No migrations found"
buildAndRunPlan migs mVersion cmd = do
  from <- readSchemaVersion
  let to   = case (mVersion, cmd) of
             (Just v', _)            -> v'
             (Nothing, "rollback":_) -> previousVersion from migs
             _                       -> mId $ last migs
  let plan = migrationPlan (Range from to) migs
  runPlan to plan

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
  migs <- readMigrations opts
  conn <- connect $ dbConnection opts

  let mVersion = version opts

  case (cmds, errors) of
    ([], _:_) -> putStrLn "an error occured"
    (_ , _)   -> runReaderT (buildAndRunPlan migs mVersion cmds) conn
