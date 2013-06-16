module Egress.Migration (
    migrations
  , migrationPlan
  , up
  , down
) where

import Data.Maybe
import Data.List (sort)
import Text.Regex.PCRE
import Egress.TypeDefs

migrations :: [FilePath] -> [Migration]
migrations = sort . mapMaybe toMigration

toMigration :: FilePath -> Maybe Migration
toMigration path =
  let
    regex             = "(^[0-9]+)-.*(up|down)(\\.sql)$"
    (_ ,_ ,_, groups) = path =~ regex :: (String, String, String, [String])
  in case groups of
    migId:"up":_   -> Just $ buildM migId Up
    migId:"down":_ -> Just $ buildM migId Down
    _ -> Nothing
  where
    buildM v updown = Migration (read v :: Int ) updown path

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

migrationPlan :: Range -> [Migration] -> [Migration]
migrationPlan r@(Range from to) = case from `compare` to of
                                    EQ -> const []
                                    LT -> up r
                                    GT -> down r
