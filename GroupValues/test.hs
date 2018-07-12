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
            groupValues ["Test", "Estt", "Nope"]  `shouldBe` [["Test", "Estt"], ["Nope"]]
        it "gives empty list when empty list is given" $ do
            groupValues []  `shouldBe` []
        it "works like task should" $ do
            a `shouldBe` b
                where
                a = groupValues ["Tokyo", "London", "Rome", "Donlon", "Kyoto", "Paris"]
                b = [["Tokyo","Kyoto"],["London","Donlon"],["Rome"], ["Paris"]]
