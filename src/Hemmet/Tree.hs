module Hemmet.Tree
    ( Tree
    , Node(..)
    , Transformation
    , ToTree(..)
    ) where

import Data.Text

type Tree = [Node]

data Node = Node
    { _nTagName :: Text
    , _nClasses :: [Text]
    , _nVars :: [Text]
    , _nChilds :: Tree
    } deriving (Show, Eq)

type Transformation = Tree -> Tree

class ToTree a where
    toTree :: a -> Tree
