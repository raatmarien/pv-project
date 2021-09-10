module ProgramPathTests where

import ProgramPath
import GCLParser.GCLDatatype
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

instance Arbitrary Stmt where
  arbitrary = QC.oneof [
    return Skip,
    Assert <$> arbitrary,
    Assume <$> arbitrary,
    Assign <$> arbitrary <*> arbitrary,
    Seq <$> arbitrary <*> arbitrary,
    IfThenElse <$> arbitrary <*> arbitrary <*> arbitrary,
    While <$> arbitrary <*> arbitrary,
    Block [] <$> arbitrary]

instance Arbitrary Expr where
  -- We don't need complicated expressions for these tests
  arbitrary = return $ LitB True

testSkipPaths = QC.testProperty "test that skip creates valid paths"
                $ \k -> generateProgramPaths k Skip == [[]]

testZeroK = QC.testProperty "test that k=0 always returns no paths if the statement is not Skip"
            $ \stmt -> stmt /= Skip
                       QC.==> (generateProgramPaths 0 stmt == [])


programPathTests = testGroup "Program path tests" [testSkipPaths, testZeroK]
