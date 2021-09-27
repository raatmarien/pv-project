import ProgramPathTests

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Tests" $ do
    programPathTests
