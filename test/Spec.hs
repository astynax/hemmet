import Data.Text as T
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Text.Megaparsec

import qualified Hemmet.Dom.Template as Dom
import Hemmet.BEM.Template as BEM
import Hemmet.BEM.Tree
import Hemmet.Tree

import Golden

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = do
  us <- testGroup "Unit tests" <$> makeUnitTests
  gs <- testGroup "Golden tests" <$> makeGoldenTests "test/tests"
  return $ testGroup "Tests" [us, gs]

makeUnitTests :: IO [TestTree]
makeUnitTests =
  testSpecs $ do
    domParserSpec
    bemParserSpec
    transformerSpec

domParserSpec :: Spec
domParserSpec =
  describe "parse BEM.template" $ do
    it "parses multiplicity" $ do
      "a>b*2" `shouldMean` [Dom.Single $ tag "a" [Dom.Times 2 $ tag "b" []]]
    where
      shouldMean s bs = q s `shouldBe` Just (Dom.Template bs)
      q = either (const Nothing) Just . parse Dom.template "foo"
      tag name cs = Dom.Tag
        { _tName = name
        , _tId = Nothing
        , _tClasses = []
        , _tChildren = Dom.Children cs
        }

bemParserSpec :: Spec
bemParserSpec =
  describe "parse BEM.template" $ do
    it "parses single block" $ do
      "div:foo" `shouldMean` tb "div" "foo" [] []
      ":foo" `shouldMean` tb "" "foo" [] []
    it "parses single element" $ do
      ":b>span.xyz" `shouldMean` te "span" "xyz" [] []
      ":b>.xyz" `shouldMean` te "" "xyz" [] []
    it "parses block/element modifiers" $ do
      ":b~m1~m2" `shouldMean` tb "" "b" [Mod "m1", Mod "m2"] []
      ":b>.e~m1~m2" `shouldMean` te "" "e" [Mod "m1", Mod "m2"] []
    it "parses block/element vars" $ do
      ":b$m1$m2" `shouldMean` tb "" "b" [Var "m1", Var "m2"] []
      ":b>.e$m1$m2" `shouldMean` te "" "e" [Var "m1", Var "m2"] []
    it "ensures proper naming" $ do
      shouldMean "_underscored:x" $ tb "_underscored" "x" [] []
      shouldFail "1leadingDigit:x"
      -- block names
      shouldFail ":snake_case"
      shouldFail ":uPPercase"
      shouldFail ":1leading-digit"
      -- elem names
      shouldFail ":b>snake_case"
      shouldFail ":b>uPPercase"
      shouldFail ":b>1leading-digit"
      -- modifier names
      shouldMean ":b~snake_case" $ tb "" "b" [Mod "snake_case"] []
      shouldFail ":b~uPPer_case"
      shouldFail ":b~1leading-digit"
      -- var names
      shouldFail ":b$kebab-case"
      shouldMean ":b$_underscored" $ tb "" "b" [Var "_underscored"] []
      shouldMean ":b$uPPer_case" $ tb "" "b" [Var "uPPer_case"] []
      shouldFail ":b$1leading-digit"
    it "parses a chain of items" $ do
      ":a+:b" `shouldMean` (b "a" [] ++ b "b" [])
      "(:a+:b)" `shouldMean` (b "a" [] ++ b "b" [])
      ":a>.e1+.e2" `shouldMean` b "a" [e "e1", e "e2"]
      ":a>(.e1+.e2)" `shouldMean` b "a" [e "e1", e "e2"]
      ":a>(.e1)+:b" `shouldMean` (b "a" [e "e1"] ++ b "b" [])
    it "parses an element+block mix" $
      ":b>.be:s>.se" `shouldMean`
      b "b" [Left . ElementBlock "be" [] $ Params "" "s" [] [e "se"]]
    it "parses an element+block mix with addons" $
      ":b>.e~em:s~sm" `shouldMean`
        b "b"
          [Left . ElementBlock "e" [Mod "em"] $
             Params "" "s" [Mod "sm"] []]
    it "parses a complex example" $
      q bemExampleQuery `shouldBe` Just bemExampleTemplate
  where
    shouldFail s = q s `shouldBe` Nothing
    shouldMean s bs = q s `shouldBe` Just (Template bs)
    q = either (const Nothing) Just . parse BEM.template "foo"
    -- shortcuts
    el t n as cs = Left . Element $ Params t n as cs
    tb t n as cs = [Block $ Params t n as cs]
    te t n as cs = tb "" "b" [] [el t n as cs]
    b c = tb "" c []
    e c = el "" c [] []

-- "transform" spec
transformerSpec :: Spec
transformerSpec =
  describe "Hemmet.toTree" $
    it "transformes a complex example" $
      toTree bemExampleTemplate `shouldBe` bemExampleNodes

-- complex examples
bemExampleQuery :: Text
bemExampleQuery =
  "form:search-form$theme>\
     \input.query>\
       \(div.help~hidden_t)\
     \+\
     \span.submit-button~disabled_t:button~text_small\
       \>.hint"

bemExampleTemplate :: Template
bemExampleTemplate =
  Template
    [ Block
      $ Params "form" "search-form" [Var "theme"]
        [ Left $ Element
          $ Params "input" "query" []
            [Left $ Element $ Params "div" "help" [Mod "hidden_t"] []]
        , Left
          $ ElementBlock "submit-button" [Mod "disabled_t"]
            $ Params "span" "button" [Mod "text_small"]
              [Left $ Element $ Params "" "hint" [] []]
        ]
    ]

bemExampleNodes :: Tree BemPayload
bemExampleNodes =
  BemPayload [] []
    [ node "form" ["search-form"] ["theme"]
      [ node "input" ["search-form__query"] []
        [ node "div" ["search-form__help", "search-form__help_hidden_t"] []
          []
        ]
      , node "span"
        [ "button"
        , "button_text_small"
        , "search-form__submit-button"
        , "search-form__submit-button_disabled_t"
        ] []
        [node "" ["button__hint"] [] []]
      ]
    ]
  where
    node n cs vs ns = Node n (BemPayload cs vs ns)
