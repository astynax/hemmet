{-# LANGUAGE ExistentialQuantification #-}

module Hemmet.Backend
    ( Backend(..)
    ) where

import Data.Text
import Text.Parsec.Text

import Hemmet.Tree

data Backend a = forall b. ToTree b a =>
                           Backend
    { getTransformation :: Text -> (Transformation a, Text)
    , parser :: Parser b
    }
