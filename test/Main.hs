import ProgramPathTests
import Test.Tasty

main = defaultMain tests

tests = testGroup "Tests" [programPathTests]
