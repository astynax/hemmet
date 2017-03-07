import Lib
import Test.Hspec

main :: IO ()
main =
    hspec $ do
        testParser
        testTransformer

testParser :: Spec
testParser =
    describe "Lib.parse" $ do
        it "parses single block" $ do
            "div:foo" `shouldMean` tb "div" "foo" [] [] []
            ":foo" `shouldMean` tb "" "foo" [] [] []
        it "parses single element" $ do
            ":b>span.xyz" `shouldMean` te "span" "xyz" [] [] []
            ":b>.xyz" `shouldMean` te "" "xyz" [] [] []
        it "parses block/element modifiers" $ do
            ":b~m1~m2" `shouldMean` tb "" "b" ["m1", "m2"] [] []
            ":b>.e~m1~m2" `shouldMean` te "" "e" ["m1", "m2"] [] []
        it "parses block/element mixes" $ do
            ":b^m1^m2" `shouldMean` tb "" "b" [] ["m1", "m2"] []
            ":b>.e^m1^m2" `shouldMean` te "" "e" [] ["m1", "m2"] []
        it "ensures proper naming" $ do
            shouldFail "_badStart:x"
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
            shouldMean ":b~snake_case" $ tb "" "b" ["snake_case"] [] []
            shouldFail ":b~uPPer_case"
            shouldFail ":b~1leading-digit"
            -- mix names
            shouldFail ":b^snake_case"
            shouldFail ":b^uPPer_case"
            shouldFail ":b^1leading-digit"
        it "parses a chains of items" $ do
            ":a+:b" `shouldMean` (tb "" "a" [] [] [] ++ tb "" "b" [] [] [])
            "(:a+:b)" `shouldMean` (tb "" "a" [] [] [] ++ tb "" "b" [] [] [])
            ":a>.e1+.e2" `shouldMean`
                tb "" "a" [] [] [elem "" "e1" [] [] [], elem "" "e2" [] [] []]
            ":a>(.e1+.e2)" `shouldMean`
                tb "" "a" [] [] [elem "" "e1" [] [] [], elem "" "e2" [] [] []]
            ":a>(.e1)+:b" `shouldMean`
                (tb "" "a" [] [] [elem "" "e1" [] [] []] ++ tb "" "b" [] [] [])
        it "parses a complex example" $
            q exampleQuery `shouldBe` Just exampleTemplate
  where
    shouldFail s = q s `shouldBe` Nothing
    shouldMean s bs = q s `shouldBe` Just (Template bs)
    q = either (const Nothing) Just . parse template "foo"
    -- shortcuts
    block t n ms mxs es = Right . Block $ params t n ms mxs es
    elem t n ms mxs es = Left . Element $ params t n ms mxs es
    tb t n ms mxs es = [Block $ params t n ms mxs es]
    te t n ms mxs es = tb "" "b" [] [] [elem t n ms mxs es]
    params t n ms mxs = Params t n (map Modifier ms) (map Mix mxs)

-- "transform" spec
testTransformer :: Spec
testTransformer =
    describe "Lib.transform" $
    it "transformes a complex example" $
    transform exampleTemplate `shouldBe` exampleNodes

-- complex examples
exampleQuery :: String
exampleQuery =
    "form:search-form>\
       \input.query^red-text>\
         \(div.hint~hidden_t)\
       \+\
       \input:button~text_small"

exampleTemplate :: Template
exampleTemplate =
    Template
        [ Block $
          Params
              "form"
              "search-form"
              []
              []
              [ Left $
                Element $
                Params
                    "input"
                    "query"
                    []
                    [Mix "red-text"]
                    [ Left $
                      Element $ Params "div" "hint" [Modifier "hidden_t"] [] []
                    ]
              , Right $
                Block $ Params "input" "button" [Modifier "text_small"] [] []
              ]
        ]

exampleNodes :: [Node]
exampleNodes =
    [ Node
          "form"
          ["search-form"]
          [ Node
                "input"
                ["search-form__query", "red-text"]
                [ Node
                      "div"
                      ["search-form__hint", "search-form__hint_hidden_t"]
                      []
                ]
          , Node "input" ["button", "button_text_small"] []
          ]
    ]
