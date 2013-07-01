module Egress.Migration 
  ( Range(..)
  , Migration(..)
  , Direction(..)
  , migrations
  , migrationPlan
  , up
  , down
  , previousVersion
) where

import Data.Maybe
import Data.List (sort, partition)
import System.FilePath (splitFileName)
import Text.Regex.PCRE

data Direction  = Up 
                | Down deriving (Show, Eq)

data Range      = Range Int Int deriving (Show)

data Migration  = Migration
  { mId         :: Int
  , mDirection  :: Direction
  , mPath       :: FilePath
  } deriving (Show, Eq)

instance Ord Migration where
  compare x y = mId x `compare` mId y

migrations :: [FilePath] -> [Migration]
migrations = sort . mapMaybe toMigration

toMigration :: FilePath -> Maybe Migration
toMigration fullpath =
  let
    regex             = "(^[0-9]+)-.*(up|down)(\\.sql)$"
    (_, path)         = splitFileName fullpath
    (_ ,_ ,_, groups) = path =~ regex :: (String, String, String, [String])
  in case groups of
    migId:"up":_   -> Just $ buildM migId Up
    migId:"down":_ -> Just $ buildM migId Down
    _              -> Nothing
  where
    buildM v updown = Migration (read v :: Int ) updown fullpath

up :: Range -> [Migration] -> [Migration]
up (Range from to) = filter isGreater . sort
  where
    isGreater (Migration mid Up _) = mid > from && mid <= to
    isGreater _ = False

down :: Range -> [Migration] -> [Migration]
down (Range from to) = filter isLower . reverse . sort
  where
    isLower (Migration mid Down _) = mid <= from && mid > to
    isLower _ = False

previousVersion :: Int -> [Migration] -> Int
previousVersion _ []   = 0
previousVersion 0 _    = 0
previousVersion v migs = case partition (< v) mIds of
                           ([], _) -> 0
                           (ids, _) -> last ids
  where
    mIds = map mId migs

migrationPlan :: Range -> [Migration] -> [Migration]
migrationPlan r@(Range from to) = case from `compare` to of
                                    EQ -> const []
                                    LT -> up r
                                    GT -> down r
