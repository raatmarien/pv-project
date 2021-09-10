module ProgramPathTests where

import ProgramPath
import GCLParser.GCLDatatype
import Test.HUnit

testSkipPaths = "test that skip creates valid paths" ~: TestList [
  [[]] ~=? generateProgramPaths 0 Skip,
  [[]] ~=? generateProgramPaths 43 Skip
  ]

programPathTests = TestList [testSkipPaths]
