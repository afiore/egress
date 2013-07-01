module Egress.MigrationSpec (
  test
  ) where

import Test.Hspec
import Egress.Migration

test = do
  let fs'  = ["some.sh", "other.up.sql"]
      fs   = [ "01-m.up.sql"
             , "01-m.down.sql"
             , "011-m.up.sql"
             , "011-m.down.sql"
             , "03-m.up.sql"
             , "README.md" ]

      migrs = migrations fs

  describe "migrations" $ do
    it "excludes files that do not match the expected pattern" $ do
      (migrations fs') `shouldBe` []

    it "correctly parses and sorts migration file names" $ do
      migrs `shouldBe` [ Migration 1 Up   "01-m.up.sql"
                       , Migration 1 Down "01-m.down.sql"
                       , Migration 3 Up   "03-m.up.sql"
                       , Migration 11 Up  "011-m.up.sql"
                       , Migration 11 Down "011-m.down.sql"]

    it "does not chop off the path basename if supplied" $ do
      migrations ["ms/01-m.up.sql"] `shouldBe` [ Migration 1 Up "ms/01-m.up.sql" ]

  describe "up" $ do
    it "selects upgrade migrations within the suppied range" $ do
      up (Range 2 11) migrs `shouldBe`  [ Migration 3 Up "03-m.up.sql"
                                         , Migration 11 Up "011-m.up.sql" ]
      up (Range 11 11) migrs `shouldBe` []

  describe "down" $ do
    it "selects downgrade migrations within the supplied range" $ do
      down (Range 11 1) migrs `shouldBe` [ Migration 11 Down "011-m.down.sql" ]
      down (Range 1 0)  migrs `shouldBe` [ Migration 1 Down "01-m.down.sql" ]

  describe "migrationPlan" $ do
    it "returns an empty list of migrations when an empty range is supplied" $ do
      migrationPlan (Range 11 11) migrs `shouldBe` []

    it "returns upgrade migrations when an 'upward' range is supplied" $ do
      migrationPlan (Range 1 11) migrs `shouldBe` [ Migration 3 Up "03-m.up.sql"
                                                  , Migration 11 Up "011-m.up.sql" ]

    it "returns downgrade migrations when a 'downwards' range is supplied" $ do
      migrationPlan (Range 11 3) migrs `shouldBe` [ Migration 11 Down "011-m.down.sql" ]

  describe "previousVersion" $ do
    context "when no migrations" $ do
      it "returns zero" $ do
        previousVersion 10 [] `shouldBe` 0
    context "when current version is zero" $ do
      it "returns zero" $ do
        previousVersion 0 migrs `shouldBe` 0
    context "when current version is not zero" $ do
      it "returns the previous version number" $ do
        previousVersion 11 migrs `shouldBe` 3
        previousVersion 3 migrs  `shouldBe` 1
        previousVersion 1 migrs  `shouldBe` 0
