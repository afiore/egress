module Egress.TypeDefs where

data Direction = Up | Down deriving (Show, Eq)
data Range     = Range Int Int
data Migration = Migration { mId         :: Int
                           , mDirection  :: Direction
                           , mPath       :: FilePath
                           } deriving (Show, Eq)

instance Ord Migration where
  compare x y = mId x `compare` mId y
