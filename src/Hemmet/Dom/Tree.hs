{-# LANGUAGE DeriveTraversable #-}

module Hemmet.Dom.Tree where

import Data.Text

import Hemmet.Tree

type DomTree = Tree DomPayload

data DomPayload a = DomPayload
  { _dpId       :: !(Maybe Text)
  , _dpClasses  :: ![Text]
  , _dpChildren :: !(Children a)
  }
  deriving (Show, Eq)

data Children a
  = Void
  | Children ![a]
  deriving (Show, Eq, Functor, Foldable, Traversable)
