module ZipperSpec where

import Control.Monad (void)
import Data.Char (isLetter)

import Test.Hspec

import Hemmet.Megaparsec
import Hemmet.Zipper

spec :: Spec
spec = do
  describe "lists and non-nesting blocks" $ do
    it "parses a single item" $ do
      run "a" `shouldBe` "a"
    it "parses non-nested lists" $ do
      run "a+b+c" `shouldBe` "a b c"
    it "parses a single block" $ do
      run "(a)" `shouldBe` "a"
    it "parses a set of blocks" $ do
      run "(a)(b)(c)" `shouldBe` "a b c"
    it "parses a mix of blocks and singles" $ do
      run "a(b)c(d)(e)f" `shouldBe` "a b c d e f"
    it "parses a blocks with lists inside" $ do
      run "(a+b)c(d+e)" `shouldBe` "a b c d e"
    it "parsed blocks of blocks" $ do
      run "(((a)(b))(c))" `shouldBe` "a b c"
  describe "going down" $ do
    it "descends" $ do
      run "a>b>c" `shouldBe` "(a (b c))"
      run "(a>(b>(c)))" `shouldBe` "(a (b c))"
    it "descends in a mix with lists" $ do
      run "a>b+c>d+e" `shouldBe` "(a b (c d e))"
  describe "going up" $ do
    it "goes one level up at time" $ do
      run "a>b^c>d" `shouldBe` "(a b) (c d)"
    it "goes up a couple of times in a row" $ do
      run "a>b>c^^d" `shouldBe` "(a (b c)) d"
    it "goes up inside blocks" $ do
      run "a>(b>c^d)e" `shouldBe` "(a (b c) d e)"
  describe "a big example" $ do
    let expected = "q (k (a (b c) (j k (r (l l)) m) d g (e f)))"
    it "runs a big but terse example" $ do
      run "q+k>a>(b>c^j>k+r>l>l^^m)d+g(e>f)" `shouldBe` expected
    it "runs a big but noisy example" $ do
      run "((q)(k>(a>(b>(c))(j>(k)(r>(l>(l)))(m))(d)(g)(e>(f)))))"
        `shouldBe` expected

data Tree = Node Char [Tree] deriving Show

treeParsing :: StepParsing Tree
treeParsing = StepParsing
  { downP         = void $ char '>'
  , upP           = void $ char '^'
  , nextP         = void $ char '+'
  , leftBracketP  = void $ char '('
  , rightBracketP = void $ char ')'
  , chunkP        = ($ []) . Node <$> satisfy isLetter
  }

instance Buildable Tree where
  root = Node '?' []
  cons (Node c _) = Node c

dump :: [Tree] -> String
dump = unwords . map dumpNode
  where
    dumpNode (Node c []) = [c]
    dumpNode (Node c ns) =
      "(" <> [c] <> " " <> unwords (map dumpNode ns) <> ")"

run :: Text -> String
run t = case parseWith treeParsing t of
  Left e -> error $ show e
  Right ss -> dump ss
