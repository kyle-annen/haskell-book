import Chapter15
import Test.Hspec

main :: IO ()
main = 
    hspec $ do
        Chapter15.spec
