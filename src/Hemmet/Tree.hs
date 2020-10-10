{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Hemmet.Tree
  ( Tree
  , Node(..)
  , Transformation
  , ToTree(..)
  ) where

import Data.Text

type Tree a = a (Node a)

data Node a = Node
  { _nName :: Text
  , _nPayload :: Tree a
  }

instance Eq (Tree a) => Eq (Node a) where
  (Node n1 p1) == (Node n2 p2) = n1 == n2 && p1 == p2

instance Show (Tree a) => Show (Node a) where
  show (Node n p) = Prelude.unwords ["Node", show n, show p]

type Transformation a = Tree a -> Tree a

class ToTree b a where
  toTree :: b -> Tree a
