{-# LANGUAGE ExistentialQuantification #-}

module Hemmet.Backend
    ( Backend(..)
    , runHemmet
    ) where

import Data.Text as T
import Text.Parsec
import Text.Parsec.Text

import Hemmet.Rendering
import Hemmet.Tree

data Backend a = forall b. ToTree b a =>
                           Backend
    { getTransformation :: Text -> (Transformation a, Text)
    , parser :: Parser b
    }

runHemmet :: Backend a -> Renderer a -> Text -> Either ParseError Text
runHemmet (Backend getTransformation' parser') renderer input =
    let (padding, preinput) = T.span (== ' ') input
        (transform, datum) = getTransformation' preinput
    in case parse parser' "template" datum of
           Right tpl ->
               Right $
               runRenderM (renderer . transform $ toTree tpl) (T.length padding)
           Left err -> Left err
