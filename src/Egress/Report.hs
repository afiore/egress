module Egress.Report
  ( printReport
  , die
  ) where

import Control.Egress
import Control.Monad (when)

import System.IO
import System.Exit

isError :: CliMessage -> Bool
isError (SuccessfulRunMessage _) = False
isError (FailedRunMessage   _ _) = True

printReport :: EgressState -> IO ()
printReport (EgressState _ msgs _) = do
  mapM_ (putStrLn . show) msgs
  when (any isError msgs) $ exitWith (ExitFailure 1)

die :: String -> IO ()
die msg = hPutStr stderr msg >> exitWith (ExitFailure 1)
