module Hemmet.Megaparsec
  ( module M
  , Void
  , Text
  , Parser
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec as M
import Text.Megaparsec.Char as M

type Parser = Parsec Void Text
