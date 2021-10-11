{-# OPTIONS_GHC
  -Wno-unused-top-binds
  -Wno-unused-imports
#-}

import BoundedVerification (boundedVerification, mergeResults)
import GCLUtils (parseGCLstring, mutateProgram)
import Gcl (Type (PType), PrimitiveType (PTBool), Expression (IntegerLiteral), Identifier)
import Gcl qualified
import Path qualified
import Tree (Tree (Empty))
import Tree qualified
import SymbolicExecution (Value, Certainty (Certain), counterExample, symbolifyPath)

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

spec :: Spec
spec = do
  describe "renaming" $ do
    it "works" $
      (
        fmap Gcl.rename $
        (=<<) Gcl.fromParseResult $
        parseGCLstring $
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
    verify "memberOf.gcl" 4 30
    verify "divByN.gcl" 3 50
    verify "pullUp.gcl" 5 100
    verify "bsort.gcl" 4 32
  where
    verify program n k = describe program $ do
      it ("verifies for N=" ++ (show n)) $
        withoutMutations ("test/examples/benchmark/" ++ program) n k
        `shouldReturn`
        Right (Left Certain)
      it ("mutations fail for N=" ++ show n) $
        (=<<) (`shouldSatisfy` all isRight) $
        fmap (take 4) $
        (withMutations ("test/examples/benchmark/" ++ program) n k)


-- -- $> pPrintLightBg =<< withoutMutations "test/examples/benchmark/bsort.gcl" 4 32

-- -- $> pPrintLightBg =<< withMutations "test/examples/benchmark/bsort.gcl" 4 32

adHocTest :: IO ()
adHocTest =
  (=<<) pPrintLightBg $
  (fmap . fmap)
    (
      length .
      -- (foldr mergeResults (Left Certain)) . -- short circuit
      -- fmap SymbolicExecution.counterExample .
      Tree.leaves .
      -- (fromMaybe Empty) .
      -- Tree.pruneByFeasibility .
      Tree.pathsTree .
      (fromMaybe Empty) .
      (Tree.pruneByLength 50) .
      Tree.statementTree .
      Gcl.addArrayAssignAssertions .
      Gcl.addIndexingAssertions .
      Gcl.rename
    ) $
  (fmap . fmap) (Gcl.instantiateN $ IntegerLiteral 3) $
  fmap Gcl.fromParseResult $
  fmap (fromRight $ error "not testing the parser") $
  fmap parseGCLstring $
  fmap decodeUtf8 $
  readFileBS "test/examples/benchmark/divByN.gcl"

-- -- $> adHocTest

renamedCorrectly :: Either String [Gcl.Statement] -> Bool
renamedCorrectly
  (Right
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
  )
  =
    a0 != a1 &&
    a0 != a4 &&
    a0 == a3 &&
    a1 == a2 &&
    a4 == a5 &&
    b0 == b1 &&
    a1 != a4
renamedCorrectly _ = False

withoutMutations ::
  String ->
  Integer ->
  Integer ->
  IO (Either String (Either Certainty (HashMap Identifier Value)))
withoutMutations file nSubstitute searchDepth =
  (fmap . fmap) (boundedVerification searchDepth) $
  (fmap . fmap) (Gcl.instantiateN $ IntegerLiteral nSubstitute) $
  fmap Gcl.fromParseResult $
  fmap (fromRight $ error "not testing the parser") $
  fmap parseGCLstring $
  fmap decodeUtf8 $
  readFileBS file

withMutations ::
  String ->
  Integer ->
  Integer ->
  IO ([Either String (Either Certainty (HashMap Identifier Value))])
withMutations file nSubstitute searchDepth =
  (fmap . fmap . fmap) (boundedVerification searchDepth) $
  (fmap . fmap . fmap) (Gcl.instantiateN $ IntegerLiteral nSubstitute) $
  (fmap . fmap) Gcl.fromParseResult $
  (fmap . fmap) snd $
  fmap mutateProgram $
  fmap (fromRight $ error "not testing the parser") $
  fmap parseGCLstring $
  fmap decodeUtf8 $
  readFileBS file

symbolifyAndShow ::
  [Path.Statement] -> String
symbolifyAndShow statements =
  unsafePerformIO $ evalZ3 $ do
    (z3Ast, _) <- symbolifyPath statements
    astToString z3Ast
    
-- test :: IO ()
-- test =
--   (=<<) pPrintLightBg $
--   (fmap . fmap) (boundedVerification 33) $
--   fmap decodeUtf8Strict $
--   readFileBS "test/examples/benchmark/bsort.gcl"
-- -- double free or corruption (!prev)
-- -- Aborted (core dumped)

-- -- double free or corruption (out)
-- -- Aborted (core dumped)

-- -- Segmentation fault (core dumped)

-- -- Rightbounded-verification-exe: Z3 error: select requires 0 arguments, but was provided with 2 arguments

main :: IO ()
main = hspec spec

hspecProgress :: Spec -> IO ()
hspecProgress spec =
  evaluateSummary
    =<< runSpec spec (defaultConfig {configFormatter = Just progress})

-- $> hspecProgress spec
