import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "split strings to list" $ do
        it "tes 1" $ do
            (3 + 5)  `shouldBe` 8
