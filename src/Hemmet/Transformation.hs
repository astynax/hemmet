module Hemmet.Transformation
    ( stripTopNode
    ) where

import Hemmet.Tree

stripTopNode :: Transformation
stripTopNode [] = []
stripTopNode (n:_) = _nChilds n
