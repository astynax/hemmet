module Hemmet.Runner
    ( Runner(..)
    , Result(..)
    , runHemmet
    ) where

import Data.Text as T
import Text.Parsec

import Hemmet.Backend
import Hemmet.Rendering
import Hemmet.Tree

data Runner a
    = PureRunner (Renderer a)
    | EffectfulRunner (Int -> Tree a -> IO ())

data Result
    = Pure Text
    | Effect (IO ())

runHemmet :: Backend a -> Runner a -> Text -> Either ParseError Result
runHemmet (Backend getTransformation' parser') runner input =
    let (prepadding, preinput) = T.span (== ' ') input
        padding = T.length prepadding
        (transform, datum) = getTransformation' preinput
    in case parse parser' "template" datum of
           Left err -> Left err
           Right tpl ->
               let tree = transform $ toTree tpl
               in Right $
                  case runner of
                      PureRunner render ->
                          Pure $ runRenderM (render tree) padding
                      EffectfulRunner run -> Effect $ run padding tree
