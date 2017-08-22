{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hemmet.FileTree.Tree where

import Hemmet.Tree

type FileTree = Tree FileTreePayload

data FileTreePayload a
    = File
    | Directory [a]

deriving instance Eq a => Eq (FileTreePayload a)

deriving instance Show a => Show (FileTreePayload a)

instance ToTree FileTree FileTreePayload where
    toTree = id
