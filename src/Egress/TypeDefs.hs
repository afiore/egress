module Egress.TypeDefs where

data Direction = Up | Down deriving (Show, Eq)

data Range     = Range Int Int deriving (Show)

data Migration = Migration { mId         :: Int
                           , mDirection  :: Direction
                           , mPath       :: FilePath
                           } deriving (Show, Eq)

data CliMessage = CliInfoMsg  String         |
                  CliInternalErrorMsg String |
                  CliErrorMsg String deriving (Show, Eq)

isError :: CliMessage -> Bool
isError (CliInfoMsg  _)         = False
isError (CliErrorMsg _)         = True
isError (CliInternalErrorMsg _) = True

data EgressState a  = EgressState { connection :: a
                                  , messages   :: [CliMessage]
                                  } deriving (Show, Eq)

instance Ord Migration where
  compare x y = mId x `compare` mId y
