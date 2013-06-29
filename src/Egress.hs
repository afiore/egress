import System.Directory
import System.FilePath((</>))
import System.Environment
import System.Console.GetOpt
import Egress.TypeDefs
import Egress.Migration
import Egress.DB
import Egress.Options
import Control.Monad.State

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

  let mVersion = version opts
  migs <- readMigrations opts
  conn <- connect $ dbConnection opts

  case conn of
    (Left _)       -> putStrLn "Cannot connect to the database"
    (Right dbconn) -> do
      case (cmds, errors) of
        ([], _:_) -> mapM_ putStrLn errors
        (_ , _)   -> do
                     (_, s) <- runStateT (buildAndRunPlan migs mVersion cmds) $ EgressState dbconn []
                     mapM_ (putStrLn . show) $ messages s
