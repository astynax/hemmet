{-# LANGUAGE ExistentialQuantification #-}

module Hemmet.Backend
  ( Backend(..)
  ) where

import Data.Text

import Hemmet.Megaparsec
import Hemmet.Tree

data Backend a = forall b. ToTree b a =>
  Backend
    { getTransformation :: Text -> (Transformation a, Text)
    , parser            :: Parser b
    , examples          :: [(Text, Text)]
      -- ^ examples in form (description, input)
    }
