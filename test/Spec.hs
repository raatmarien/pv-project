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
  describe "examples" $ do
      verify "benchmark/bsort.gcl" 4 36
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

-- double free or corruption (!prev)
-- Aborted (core dumped)

-- double free or corruption (out)
-- Aborted (core dumped)

-- Segmentation fault (core dumped)

-- Rightbounded-verification-exe: Z3 error: select requires 0 arguments, but was provided with 2 arguments

-- malloc(): smallbin double linked list corrupted

-- ASSERTION VIOLATION
-- File: ../src/ast/ast.cpp
-- Line: 450
-- UNEXPECTED CODE WAS REACHED.
-- Z3 4.8.10.0
-- Please file an issue with this message and more detail about how you encountered it at https://github.com/Z3Prover/z3/issues/new
-- double free or corruption (fasttop)


-- ASSERTION VIOLATION
-- File: ../src/ast/ast.cpp
-- Line: 432
-- UNEXPECTED CODE WAS REACHED.
-- Z3 4.8.10.0
-- Please file an issue with this message and more detail about how you encountered it at https://github.com/Z3Prover/z3/issues/new

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

-- -- $> hspecProgress spec
