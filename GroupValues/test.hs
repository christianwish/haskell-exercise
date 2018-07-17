import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import GroupValues (groupValues)

main :: IO ()
main = hspec $ do
    describe "groupValues" $ do
        context "[String] -> [[String]]" $ do
        it "groups all when values are same" $ do
            groupValues ["Test", "Estt"]  `shouldBe` [["Test", "Estt"]]
        it "groups single values when nothing matches" $ do
            groupValues ["Test", "Estt", "Nope"]  `shouldBe` [["Nope"],["Test","Estt"]]
        it "gives empty list when empty list is given" $ do
            groupValues []  `shouldBe` []
        it "works like it should" $ do
            actual `shouldBe` expected
                where
                actual = groupValues ["Tokyo", "London", "Rome", "Donlon", "Kyoto", "Paris"]
                expected = [["Paris"],["London","Donlon"],["Rome"],["Tokyo","Kyoto"]]
