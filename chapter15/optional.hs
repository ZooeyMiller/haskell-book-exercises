import           Test.Hspec
import           Data.Monoid

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada x = x
    mappend x Nada = x
    mappend (Only x) (Only y) = Only $ mappend x y

main :: IO ()
main = hspec $ do
  describe "Optional monoid instance" $ do
    it "applies mappend to nested values" $ do
      Only (Sum 1) `mappend` Only (Sum 1) `shouldBe` Only (Sum {getSum = 2})
    it "works with one mempty value" $ do
      Only (Sum 1) `mappend` Nada `shouldBe` Only (Sum {getSum = 1})
    it "works with 2 mempty values" $ do
      (mappend Nada Nada :: Optional [Int]) `shouldBe` Nada
