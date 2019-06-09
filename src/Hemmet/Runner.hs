module Hemmet.Runner
    ( Runner(..)
    , Result(..)
    , runHemmet
    ) where

import Data.Text as T

import Hemmet.Backend
import Hemmet.Megaparsec
import Hemmet.Rendering
import Hemmet.Tree

data Runner a
    = PureRunner (Renderer a)
    | EffectfulRunner (Text -> Tree a -> IO Text)

data Result
    = Pure Text
    | Effect (IO Text)

runHemmet :: Backend a -> Runner a -> Text -> Either SimpleParseError Result
runHemmet (Backend getTransformation' parser' _) runner input =
    let (padding, preinput) = T.span (== ' ') input
        (transform, datum) = getTransformation' preinput
    in case parse parser' "template" datum of
           Left err  -> Left err
           Right tpl ->
               let tree = transform $ toTree tpl
               in Right $
                  case runner of
                      PureRunner render ->
                          Pure $ runRenderM (render tree) padding
                      EffectfulRunner run -> Effect $ run padding tree
