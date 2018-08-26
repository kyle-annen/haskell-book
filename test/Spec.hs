import Chapter15
import Chapter16
import Chapter17
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    Chapter15.spec
    Chapter16.spec
    Chapter17.spec
