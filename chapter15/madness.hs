
import Data.Monoid
import Test.Hspec

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
    e <> "! he said " <>
    adv <> " as he jumped into his car " <> 
    noun <> " and drove off with his " <> 
    adj <> " wife."


madlibbinBetter' :: Exclamation
                    -> Adverb
                    -> Noun
                    -> Adjective
                    -> String
madlibbinBetter' e adv noun adj = 
    mconcat [
        e, "! he said ",
        adv, " as he jumped into his car ", 
        noun, " and drove off with his ", 
        adj, " wife." 
    ]

main :: IO ()
main = hspec $ do 
    describe "madlibbin'" $ do
        it "madlibbin' and madlibbinBetter' behave the same" $ do
            madlibbinBetter' "a" "e" "i" "o" `shouldBe` madlibbin' "a" "e" "i" "o"