module Control.Egress (
  Egress
  , EgressState(..)
  , CliMessage(..)
  , readSchemaVersion
  , writeSchemaVersion
  , runMigration
  , runUpgradePlan
  , runRollbackPlan
  , setVersion
) where

import Control.Monad.State
import Control.Exception (try)

import Database.HDBC

import Egress.DB
import Egress.Migration

data CliMessage = SuccessfulRunMessage Migration
                | FailedRunMessage Migration SqlError
                deriving (Eq)

instance Show CliMessage where
  show (SuccessfulRunMessage (Migration _ _ path))                = do
    "✔ "++ "Successfully run " ++ path
  show (FailedRunMessage (Migration _ _ path) (SqlError _ _ msg)) = do
    "✗ " ++ "Error encountered while running "++ path ++ "\n" ++ msg

data EgressState a = EgressState 
  { connection  :: a
  , messages    :: [CliMessage]
  , eMigrations :: [Migration]
  }

type Egress a b = StateT (EgressState a) IO b
type SqlRecords = [[SqlValue]]

getConnection :: IConnection c => Egress c c
getConnection = get >>= return . connection

getMigrations :: IConnection c => Egress c [Migration]
getMigrations = get >>= return . eMigrations

commitDb :: IConnection c => Egress c ()
commitDb = getConnection >>= liftIO . commit >> return ()

runQuery :: IConnection c => String -> [SqlValue] -> Egress c (SafeSql SqlRecords)
runQuery q vs = do
  let quickQ = (\ conn -> quickQuery conn q vs)
  getConnection >>= liftIO . try . quickQ

writeSchemaVersion :: IConnection c => Migration -> Egress c ()
writeSchemaVersion (Migration v Down _) = do
  migs <- getMigrations
  let v' = previousVersion v migs
  writeSchemaVersion' v'
writeSchemaVersion (Migration v Up _)   = do
  writeSchemaVersion' v

writeSchemaVersion' :: IConnection c => Int -> Egress c ()
writeSchemaVersion' v = do
  (runQuery sqlUpdateVersion $ [toSql v]) >> commitDb

readSchemaVersion :: IConnection c => Egress c Int
readSchemaVersion = do
  eitherResults <- runQuery sqlSelectVersion []
  return $ case eitherResults of
    Left _        -> 0
    Right results -> case results of
                       (version:_):_ -> fromSql version
                       _             -> 0

runMigration :: IConnection c => Migration -> Egress c (SafeSql SqlRecords)
runMigration m@(Migration _ _ mpath) = do
  q         <- liftIO $ readFile mpath
  eitherRes <- runQuery q []
  case eitherRes of
    err@(Left e)  -> (logMigrationFail m e) >> return err
    res@(Right _) -> (logMigrationRun m) >> (writeSchemaVersion m) >> return res

runPlan :: IConnection c => [Migration] -> Egress c ()
runPlan []         = return ()
runPlan (mig:rest) = do
  result <- runMigration mig
  case result of
    (Left _) -> return ()
    _        -> runPlan rest

buildAndRunPlan :: IConnection c => Int -> Int -> Egress c ()
buildAndRunPlan from to = do
  migs <- getMigrations
  let range = (Range from to)
      plan  = migrationPlan range migs
  runPlan plan

setVersion :: IConnection c => Int -> Egress c ()
setVersion to = do
  from <- readSchemaVersion
  buildAndRunPlan from to

runRollbackPlan :: IConnection c => Egress c ()
runRollbackPlan = do
  from <- readSchemaVersion
  migs <- getMigrations

  let to = previousVersion from migs
  buildAndRunPlan from to

runUpgradePlan :: IConnection c => Egress c ()
runUpgradePlan = do
  from <- readSchemaVersion
  migs <- getMigrations

  let to = mId $ last migs
  buildAndRunPlan from to

appendMessage :: IConnection c => Migration -> Maybe SqlError -> EgressState c -> EgressState c
appendMessage m Nothing  s = s { messages = (messages s) ++ [SuccessfulRunMessage m] }
appendMessage m (Just e) s = s { messages = (messages s) ++ [FailedRunMessage m e] }

logMigrationRun :: IConnection c => Migration -> Egress c ()
logMigrationRun m = do
  modify (appendMessage m Nothing)

logMigrationFail :: IConnection c => Migration -> SqlError -> Egress c ()
logMigrationFail m err = do
  modify (appendMessage m (Just err))
