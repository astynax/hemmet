{-# LANGUAGE StandaloneDeriving #-}

module Hemmet.BEM.Tree where

import Data.Text

import Hemmet.Tree

type BemTree = Tree BemPayload

data BemPayload a = BemPayload
  { _bpClasses :: ![Text]
  , _bpVars    :: ![Text]
  , _bpChilds  :: ![a]
  }

deriving instance Eq a => Eq (BemPayload a)

deriving instance Show a => Show (BemPayload a)
