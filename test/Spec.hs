{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Test.Hspec

import Hemmet
import Hemmet.Template

main :: IO ()
main =
    hspec $ do
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
            ":a+:b" `shouldMean` (tb "" "a" [] [] ++ tb "" "b" [] [])
            "(:a+:b)" `shouldMean` (tb "" "a" [] [] ++ tb "" "b" [] [])
            ":a>.e1+.e2" `shouldMean`
                tb "" "a" [] [el "" "e1" [] [], el "" "e2" [] []]
            ":a>(.e1+.e2)" `shouldMean`
                tb "" "a" [] [el "" "e1" [] [], el "" "e2" [] []]
            ":a>(.e1)+:b" `shouldMean`
                (tb "" "a" [] [el "" "e1" [] []] ++ tb "" "b" [] [])
        it "parses an element-block" $ do
            ":a>.b&c>.d" `shouldMean`
                (tb
                     ""
                     "a"
                     []
                     [ Left . ElementBlock "b" $
                       Params "" "c" [] [el "" "d" [] []]
                     ])
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

exampleNodes :: [Node]
exampleNodes =
    [ Node
          "form"
          ["search-form"]
          ["theme"]
          [ Node
                "input"
                ["search-form__query", "red-text"]
                []
                [ Node
                      "div"
                      ["search-form__help", "search-form__help_hidden_t"]
                      []
                      []
                ]
          , Node
                "span"
                ["search-form__submit", "button", "button_text_small"]
                []
                [Node "" ["button__hint"] [] []]
          ]
    ]
