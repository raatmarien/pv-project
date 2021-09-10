module ProgramPathTests where

import ProgramPath
import GCLParser.GCLDatatype
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

testSkipPaths = QC.testProperty "test that skip creates valid paths"
                $ \k -> generateProgramPaths k Skip == [[]]

programPathTests = testGroup "Program path tests" [testSkipPaths]
