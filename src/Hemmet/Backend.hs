{-# LANGUAGE ExistentialQuantification #-}

module Hemmet.Backend
  ( Backend(..)
  ) where

import Data.Text

import Hemmet.Megaparsec
import Hemmet.Tree

data Backend e a = forall b. (ToTree b a, ShowErrorComponent e) =>
  Backend
    { getTransformation :: Text -> (Transformation a, Text)
    , parse             :: Text -> Either (ParseErrorBundle Text e) b
    , examples          :: [(Text, Text)]
      -- ^ examples in form (description, input)
    }
