module Hemmet.Megaparsec
    ( module M
    , Parser
    , SimpleParseError
    ) where

import Data.Text
import Data.Void
import Text.Megaparsec as M
import Text.Megaparsec.Char as M

type Parser = Parsec Void Text

type SimpleParseError = ParseError Char Void
