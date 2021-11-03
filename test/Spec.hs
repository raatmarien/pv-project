{-# OPTIONS_GHC
  -Wno-unused-top-binds
  -Wno-unused-imports
#-}

import BoundedVerification (boundedVerification, parse)
import GCLUtils (mutateProgram)
import Gcl (Type (PType), PrimitiveType (PTBool), Expression (IntegerLiteral), Identifier)
import Gcl qualified
import Path qualified
import Tree (Tree (Empty))
import Tree qualified
import SymbolicExecution (Value, counterExample, symbolifyPath)

import Data.Sequence qualified as S
import Test.Hspec
import Test.Hspec.Runner
  (runSpec, defaultConfig, evaluateSummary, configFormatter)
import Test.Hspec.Formatters (progress)
import Text.Pretty.Simple (pPrintLightBg)
import Text.RawString.QQ (r)
import System.IO.Unsafe (unsafePerformIO)
import Z3.Monad
import Std
import Relude.Unsafe qualified as U

spec :: Spec
spec = do
  describe "renaming" $ do
    it "works" $
      (
        Gcl.rename $
        Gcl.fromParseResult $
        parse $
        [r|
          f(a:bool | b:bool) {
            var a:bool {
              assert a;
              assert b
            };
            assert a;
            var a:bool {
              assert a
            }
          }
        |]
      )
      `shouldSatisfy`
      renamedCorrectly
  describe "examples" $ do
    describe "bsort" $ do
      verify "benchmark/bsort.gcl" 4 35
      it "mutations fail for N=4" $
        (=<<) (`shouldSatisfy` all isJust) $
        fmap (take 4) $
        -- line 12. "k<" -> "k<=". harmless because of "m := #a-1 ;"
        fmap (drop 1) $
        verifyMutations "benchmark/bsort.gcl" 4 35
    describe "memberOf" $ do
      verify "benchmark/memberOf.gcl" 4 30
    describe "divByN" $ do
      verify "benchmark/divByN.gcl" 3 40
    describe "pullUp" $ do
      verify "benchmark/pullUp.gcl" 5 30
    describe "min" $ do
      verify "benchmark/min.gcl" 3 25
  where
    verify program nSubstitute searchDepth =
      it ("verifies for N=" ++ show nSubstitute) $
        (`shouldReturn` Nothing) $
        fmap (boundedVerification searchDepth True) $
        fmap (Gcl.instantiateN $ IntegerLiteral nSubstitute) $
        fmap Gcl.fromParseResult $
        fmap parse $
        fmap decodeUtf8 $
        readFileBS ("test/examples/" <> program)

verifyMutations ::
  FilePath ->
  Integer ->
  Integer ->
  IO [Maybe (HashMap Identifier Value)]
verifyMutations file nSubstitute searchDepth =
  (fmap . fmap) (boundedVerification searchDepth True) $
  (fmap . fmap) (Gcl.instantiateN $ IntegerLiteral nSubstitute) $
  (fmap . fmap) Gcl.fromParseResult $
  (fmap . fmap) snd $
  fmap mutateProgram $
  fmap parse $
  fmap decodeUtf8 $
  readFileBS ("test/examples/" <> file)


-- -- $> (=<<) pPrintLightBg $ fmap (U.!! 5) $ (fmap . fmap) isRight $ verifyMutations "benchmark/bsort.gcl" 4 45

adHocTest :: IO ()
adHocTest =
  (=<<) pPrintLightBg $
  fmap
    (
      length .
      -- foldr (<|>) Nothing . -- short circuit
      -- fmap SymbolicExecution.counterExample .
      Tree.leaves .
      -- fromMaybe Empty .
      -- Tree.pruneByFeasibility .
      Tree.pathsTree .
      fromMaybe Empty .
      Tree.pruneByLength 50 .
      Tree.statementTree .
      Gcl.addArrayAssignAssertions .
      Gcl.addIndexingAssertions .
      Gcl.rename
    ) $
  fmap (Gcl.instantiateN $ IntegerLiteral 3) $
  fmap Gcl.fromParseResult $
  fmap parse $
  fmap decodeUtf8 $
  readFileBS "test/examples/benchmark/divByN.gcl"

-- -- $> adHocTest

printMutation :: IO ()
printMutation =
  (=<<) pPrintLightBg $
  fmap snd $
  fmap (U.!! 0) $
  fmap mutateProgram $
  fmap parse $
  fmap decodeUtf8 $
  readFileBS ("test/examples/benchmark/bsort.gcl")

renamedCorrectly :: [Gcl.Statement] -> Bool
renamedCorrectly
  [
    Gcl.Declarations
      (
        (a0, PType PTBool) :|
        (b0, PType PTBool) :
        []
      )
      [
        Gcl.Declarations
          ((a1, PType PTBool) :| [])
          [
            Gcl.Assert (Gcl.Variable a2),
            Gcl.Assert (Gcl.Variable b1)
          ],
        Gcl.Assert (Gcl.Variable a3),
        Gcl.Declarations
          ((a4, PType PTBool) :| [])
          [
            Gcl.Assert (Gcl.Variable a5)
          ]
      ]
  ]
  =
    a0 != a1 &&
    a0 != a4 &&
    a0 == a3 &&
    a1 == a2 &&
    a4 == a5 &&
    b0 == b1 &&
    a1 != a4
renamedCorrectly _ = False

symbolifyAndShow ::
  [Path.Statement] -> String
symbolifyAndShow statements =
  unsafePerformIO $ evalZ3 $ do
    (z3Ast, _) <- symbolifyPath statements
    astToString z3Ast

main :: IO ()
main = hspec spec

hspecProgress :: Spec -> IO ()
hspecProgress spec =
  evaluateSummary
    =<< runSpec spec (defaultConfig {configFormatter = Just progress})

-- $> hspecProgress spec
