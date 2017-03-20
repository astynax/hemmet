{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString as BS
import Data.ByteString.Lazy as BSL hiding (ByteString)
import Data.Monoid
import Data.Text as T
import Data.Text.Encoding
import System.FilePath
import System.FilePath.Glob
import Test.Hspec
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Hspec
import Text.Parsec

import Hemmet
import Hemmet.BEM.Template
import Hemmet.BEM.Tree
import Hemmet.Tree

type RendererInfo = (BemRunner, String)

renderers :: [RendererInfo]
renderers =
    [(bemHtml, ".html"), (bemCss, ".css"), (bemReactFlux, ".react-flux")]

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = do
    us <- testGroup "Unit tests" <$> mkUnitTests
    gs <- testGroup "Golden tests" <$> (mapM mkGoldenTest =<< listTestFiles)
    return $ testGroup "Tests" [us, gs]

mkUnitTests :: IO [TestTree]
mkUnitTests =
    testSpecs $ do
        testParser
        testTransformer

testParser :: Spec
testParser =
    describe "Lib.parse" $ do
        it "parses single block" $ do
            "div:foo" `shouldMean` tb "div" "foo" [] []
            ":foo" `shouldMean` tb "" "foo" [] []
        it "parses single element" $ do
            ":b>span.xyz" `shouldMean` te "span" "xyz" [] []
            ":b>.xyz" `shouldMean` te "" "xyz" [] []
        it "parses block/element modifiers" $ do
            ":b~m1~m2" `shouldMean` tb "" "b" [Mod "m1", Mod "m2"] []
            ":b>.e~m1~m2" `shouldMean` te "" "e" [Mod "m1", Mod "m2"] []
        it "parses block/element mixes" $ do
            ":b^m1^m2" `shouldMean` tb "" "b" [Mix "m1", Mix "m2"] []
            ":b>.e^m1^m2" `shouldMean` te "" "e" [Mix "m1", Mix "m2"] []
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
            -- mix names
            shouldFail ":b^snake_case"
            shouldFail ":b^uPPer_case"
            shouldFail ":b^1leading-digit"
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
        it "parses an element-block" $ do
            ":a>.b&c>.d" `shouldMean`
                (b "a" [Left . ElementBlock "b" $ Params "" "c" [] [e "d"]])
        it "parses a complex example" $
            q exampleQuery `shouldBe` Just exampleTemplate
  where
    shouldFail s = q s `shouldBe` Nothing
    shouldMean s bs = q s `shouldBe` Just (Template bs)
    q = either (const Nothing) Just . parse template "foo"
    -- shortcuts
    el t n as cs = Left . Element $ Params t n as cs
    tb t n as cs = [Block $ Params t n as cs]
    te t n as cs = tb "" "b" [] [el t n as cs]
    b c = tb "" c []
    e c = el "" c [] []

-- "transform" spec
testTransformer :: Spec
testTransformer =
    describe "Hemmet.toTree" $
    it "transformes a complex example" $
    toTree exampleTemplate `shouldBe` exampleNodes

-- complex examples
exampleQuery :: Text
exampleQuery =
    "form:search-form$theme>\
       \input.query^red-text>\
         \(div.help~hidden_t)\
       \+\
       \span.submit&button~text_small\
         \>.hint"

exampleTemplate :: Template
exampleTemplate =
    Template
        [ Block $
          Params
              "form"
              "search-form"
              [Var "theme"]
              [ Left $
                Element $
                Params
                    "input"
                    "query"
                    [Mix "red-text"]
                    [Left $ Element $ Params "div" "help" [Mod "hidden_t"] []]
              , Left $
                ElementBlock "submit" $
                Params
                    "span"
                    "button"
                    [Mod "text_small"]
                    [Left $ Element $ Params "" "hint" [] []]
              ]
        ]

exampleNodes :: Tree BemPayload
exampleNodes =
    BemPayload [] [] $
    [ node
          "form"
          ["search-form"]
          ["theme"]
          [ node
                "input"
                ["search-form__query", "red-text"]
                []
                [ node
                      "div"
                      ["search-form__help", "search-form__help_hidden_t"]
                      []
                      []
                ]
          , node
                "span"
                ["search-form__submit", "button", "button_text_small"]
                []
                [node "" ["button__hint"] [] []]
          ]
    ]
  where
    node n cs vs ns = Node n (BemPayload cs vs ns)

mkGoldenTest :: FilePath -> IO TestTree
mkGoldenTest path = do
    input <- BS.readFile path
    let baseName = takeBaseName path
    return . testGroup baseName $ Prelude.map (mkTest baseName input) renderers
  where
    mkTest testName input (renderer, suffix) =
        let goldenFile = replaceExtension path $ suffix <> ".golden"
        in goldenVsStringDiff
               (testName <> suffix)
               (\gold new -> ["diff", "-u", gold, new])
               goldenFile
               (return . BSL.fromStrict . run renderer $ input)

listTestFiles :: IO [FilePath]
listTestFiles = globDir1 pat "test/tests"
  where
    pat = compile "*.hemmet"

run :: BemRunner -> ByteString -> ByteString
run r =
    either (encodeUtf8 . T.pack . show) (encodeUtf8 . fromResult) .
    runHemmet bem r . Prelude.head . T.lines . decodeUtf8
  where
    fromResult (Pure t) = t
    fromResult _ = error "Unexpected effectful result from BEM!"
