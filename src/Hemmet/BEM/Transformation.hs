module Hemmet.BEM.Transformation where

import Hemmet.Tree

import Hemmet.BEM.Tree

stripTopNode :: Transformation BemPayload
stripTopNode x@(BemPayload _ _ [])    = x
stripTopNode   (BemPayload _ _ (n:_)) = _nPayload n
