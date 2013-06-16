import System.IO
import System.Directory
import System.Environment
import System.Console.GetOpt
import Egress.Migration
import Egress.TypeDefs
import Egress.DB
import Egress.Options

type Command = [String]

buildAndRunPlan :: Options -> Command -> IO ()
buildAndRunPlan opts cmd = do
  db    <- connect $ dbConnection opts
  paths <- getDirectoryContents $ migrationsDir opts
  sv    <- readSchemaVersion db

  let migs   = migrations paths

  let vFirst = mId $ head migs
  let vLast  = mId $ last migs

  let from = case sv of
               Just v -> v
               _      -> 0

  let to = case (version opts, cmd) of
               (Just v', _)         -> v'
               (Nothing, "down":_) ->  vFirst
               _                   ->  vLast

  let plan = migrationPlan (Range from to) migs

  runPlan db (migrationsDir opts) to plan
  return ()

main :: IO ()
main = do
  args  <- getArgs
  let (actions, cmds, errors) = getOpt Permute options args
  opts  <- foldl (>>=) (return defaultOptions) actions

  case (cmds, errors) of
    ([], _:_) -> print "an error occured"
    (_ , _)    -> buildAndRunPlan opts cmds

  return ()
