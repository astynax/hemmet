{-# LANGUAGE StandaloneDeriving #-}

module Hemmet.Dom.Tree where

import Data.Text

import Hemmet.Tree

type DomTree = Tree DomPayload

data DomPayload a = DomTag
  { _dpId      :: !(Maybe Text)
  , _dpClasses :: ![Text]
  , _dpChilds  :: ![a]
  } | DomPlainText !Text

deriving instance Eq a => Eq (DomPayload a)

deriving instance Show a => Show (DomPayload a)
