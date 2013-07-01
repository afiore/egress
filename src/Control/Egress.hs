module Control.Egress (
  readSchemaVersion
  , writeSchemaVersion
  , runMigration
  , runUpgradePlan
  , runRollbackPlan
  , setVersion
  , Egress
) where

import Control.Monad.State
import Control.Exception (try)

import Database.HDBC
import Database.HDBC.Sqlite3

import Egress.DB
import Egress.TypeDefs
import Egress.Migration

type Egress a   = StateT EgressState IO a
type SqlRecords = [[SqlValue]]

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

runPlan :: [Migration] -> Egress ()
runPlan []         = return ()
runPlan (mig:rest) = do
  result <- runMigration mig
  case result of
    (Left _) -> return ()
    _        -> runPlan rest

buildAndRunPlan :: Int -> Int -> Egress ()
buildAndRunPlan from to = do
  migs <- getMigrations
  let range = (Range from to)
      plan  = migrationPlan range migs
  runPlan plan

setVersion :: Int -> Egress ()
setVersion to = do
  from <- readSchemaVersion

  buildAndRunPlan from to

runRollbackPlan :: Egress ()
runRollbackPlan = do
  from <- readSchemaVersion
  migs <- getMigrations

  let to = previousVersion from migs
  buildAndRunPlan from to

runUpgradePlan :: Egress ()
runUpgradePlan = do
  from <- readSchemaVersion
  migs <- getMigrations

  let to = mId $ last migs
  buildAndRunPlan from to

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
