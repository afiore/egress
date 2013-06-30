module Control.Egress (
  readSchemaVersion
  , writeSchemaVersion
  , runMigration
  , buildAndRunPlan
  , Egress
) where

import Control.Monad.State
import Control.Exception (try)

import Database.HDBC
import Database.HDBC.Sqlite3

import Egress.DB
import Egress.TypeDefs
import Egress.Migration

type Egress a   = StateT (EgressState Connection) IO a
type SqlRecords = [[SqlValue]]
type Command = [String]

getConnection :: Egress Connection
getConnection = get >>= return . connection

getMigrations :: Egress [Migration]
getMigrations = get >>= return . eMigrations

commitDb :: Egress ()
commitDb = getConnection >>= liftIO . commit >> return ()

runQuery :: String -> [SqlValue] -> Egress (SafeSql SqlRecords)
runQuery q vs = do
  let quickQ = (\ conn -> quickQuery conn q vs)
  getConnection >>= liftIO . try . quickQ

writeSchemaVersion :: Migration -> Egress ()
writeSchemaVersion (Migration v Down _) = do
  migs <- getMigrations
  let v' = previousVersion v migs
  writeSchemaVersion' v'
writeSchemaVersion (Migration v Up _)   = do
  writeSchemaVersion' v

writeSchemaVersion' :: Int -> Egress ()
writeSchemaVersion' v = do
  (runQuery sqlUpdateVersion $ [toSql v]) >> commitDb

readSchemaVersion :: Egress Int
readSchemaVersion = do
  eitherResults <- runQuery sqlSelectVersion []
  return $ case eitherResults of
    Left _        -> 0
    Right results -> case results of
                       (version:_):_ -> fromSql version
                       _             -> 0

runMigration :: Migration -> Egress (SafeSql SqlRecords)
runMigration m@(Migration _ _ mpath) = do
  q         <- liftIO $ readFile mpath
  eitherRes <- runQuery q []
  case eitherRes of
    err@(Left e)  -> (logMigrationFail m e) >> return err
    res@(Right _) -> (logMigrationRun m) >> (writeSchemaVersion m) >> return res

runPlan :: Int -> [Migration] -> Egress ()
runPlan _ []         = return ()
runPlan v (mig:rest) = do
  result <- runMigration mig
  case result of
    (Left _) -> return ()
    _        -> runPlan v rest

buildAndRunPlan :: Maybe Int -> Command -> Egress ()
buildAndRunPlan mVersion cmd = do
  from <- readSchemaVersion
  migs <- getMigrations

  let to   = case (mVersion, cmd) of
               (Just v', _)            -> v'
               (Nothing, "rollback":_) -> previousVersion from migs
               _                       -> mId $ last migs
  let plan = migrationPlan (Range from to) migs
  runPlan to plan

logMigrationRun :: Migration -> Egress ()
logMigrationRun (Migration _ _ mpath) = do
  modify (\ s -> s { messages = (messages s) ++ [CliInfoMsg $ "Successfully run migration: " ++ mpath] })

logMigrationFail :: Migration -> SqlError -> Egress ()
logMigrationFail (Migration _ _ mpath) err = do
  modify (\ s -> do
    let msg = "An error occurred while running migration: " ++ mpath ++ "\n" ++ seErrorMsg err
    s { messages = (messages s) ++ [CliErrorMsg $ msg] })

--logMsg :: String -> Egress ()
--logMsg msg = modify (\ s -> s { messages = (messages s) ++ [CliInfoMsg $ msg ] })
