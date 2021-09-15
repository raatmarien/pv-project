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
    Assign "var" <$> arbitrary,
    Seq <$> arbitrary <*> arbitrary,
    IfThenElse <$> arbitrary <*> arbitrary <*> arbitrary,
    While <$> arbitrary <*> arbitrary,
    Block [] <$> arbitrary]

instance Arbitrary Expr where
  -- We don't need complicated expressions for these tests
  arbitrary = return $ LitB True

true = LitB True

testSkipPaths = QC.testProperty "test that skip creates valid paths"
                $ \k -> generateProgramPaths k Skip == [[]]

testZeroK = QC.testProperty "test that k=0 always returns no paths if the statement is not Skip"
            $ \stmt -> (not $ isSkip stmt)
                       QC.==> (generateProgramPaths 0 stmt == [])
  where isSkip (Block _ s) = isSkip s
        isSkip Skip        = True
        isSkip (Seq s1 s2) = isSkip s1 && isSkip s2
        isSkip _           = False

testNotTooLong = QC.testProperty "test that it doesn't generate too long paths"
                 $ \stmt -> do
                   k <- choose (0, 8)
                   return $ all (\x -> length x <= k)
                     $ generateProgramPaths k stmt

testCombinesIfThenElse = QC.testProperty "test that as many program paths are generated for if then else as for both branches added together"
                         $ \s1 s2 -> do
                           k <- choose (1, 4)
                           let p1 = generateProgramPaths (k-1) s1
                               p2 = generateProgramPaths (k-1) s2
                               pt = generateProgramPaths k
                                    $ IfThenElse (LitB True) s1 s2
                           return $ length pt == (length p1) + (length p2)

testIgnoresBlocks = QC.testProperty "test that blocks are ignored" $
                    \s -> do
                      k <- choose (0, 5)
                      return $ generateProgramPaths k s
                        == generateProgramPaths k (Block [] s)

testSeqMakesCartesianProduct = QC.testProperty "test that seq creates all combinations"
                               $ \s2 -> do
                                 k <- choose (0, 8)
                                 let s1 = IfThenElse true (Assume true) (Assert true)
                                     p1 = generateProgramPaths 2 s1
                                     p2 = generateProgramPaths k s2
                                     pt = generateProgramPaths (k+2) $ Seq s1 s2
                                 return $ length pt == (length p1) * (length p2)
                                 

testWhileContainsEndCase = QC.testProperty "test that while contains the end case"
                           $ \s -> do
                             k <- choose (1, 8)
                             let p = generateProgramPaths k $ While true s
                             return $ [BAssume $ OpNeg true] `elem` p

programPathTests = testGroup "Program path tests" [
  testSkipPaths,
  testZeroK,
  testNotTooLong,
  testCombinesIfThenElse,
  testIgnoresBlocks,
  testSeqMakesCartesianProduct,
  testWhileContainsEndCase
  ]