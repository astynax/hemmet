{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hemmet
    ( module Hemmet.Rendering
    , module Hemmet.Transformation
    , Template
    , Tree
    , Node(..)
    , ToTree(..)
    , Transformation
    , template
    , runHemmet
      -- reexports
    , ParseError
    , parse
    ) where

import Data.Text as T

import Hemmet.Rendering
import Hemmet.Template
import Hemmet.Transformation
import Hemmet.Tree

runHemmet :: Renderer -> Text -> Either ParseError Text
runHemmet renderer input =
    let (pad, preinput) = T.span (== ' ') input
        (transform, datum) =
            if "<" `T.isPrefixOf` preinput
                then (stripTopNode, T.tail preinput)
                else (id, preinput)
    in case parse template "template" datum of
           Right tpl ->
               Right $
               runRenderM (renderer . transform $ toTree tpl) (T.length pad)
           Left err -> Left err
