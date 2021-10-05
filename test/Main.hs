import ProgramPathTests

import Test.Hspec
import Text.RawString.QQ (r)

main :: IO ()
main = hspec $ do
  describe "Tests" $ do
    programPathTests
    it "external" $ do
      program <- readFile "examples/E.gcl"
      program `shouldBe` "hi"
    it "inline" $
      [r|
// Just a very simple example; it is faulty
E(x:int | y:int){
  assume 1<x ;
  while 0<x do{ x:=x-1 } ; y:=x ;
  assert y=1 // faulty
}
      |]
      `shouldBe`
      "hi"
