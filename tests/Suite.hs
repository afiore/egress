import qualified Egress.MigrationSpec as Migration

import Test.Hspec

main :: IO ()
main = hspec $ do
  Migration.test
