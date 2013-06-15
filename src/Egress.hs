import System.IO
import System.Directory
import System.Environment
import Egress.Migration
import Egress.TypeDefs
import Egress.DB
import Database.HDBC(IConnection)

migrationsDir :: FilePath
migrationsDir = "migrations"

runPlan :: IConnection conn => conn -> Maybe Int -> Int -> [Migration] -> IO ()
runPlan _ Nothing _ _ = do
  return ()
runPlan db (Just from) to migrs = do
  let plan = migrationPlan (Range from to) migrs
  _ <- mapM (runMigration db migrationsDir) plan
  _ <- writeSchemaVersion db to
  print plan
  return ()


main :: IO ()
main = do
  paths <- getDirectoryContents migrationsDir
  let ms = migrations paths

  args  <- getArgs
  let to = read (args !! 0) :: Int

  db    <- connect "example.sqlite3"
  from  <- readSchemaVersion db
  _ <- runPlan db from to ms
  return ()

