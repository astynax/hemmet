module Hemmet.FileTree
    ( FileTreeBackend
    , FileTreeRunner
    , fileTree
    , bashScript
    , treeLike
    ) where

import Data.Text as T

import Hemmet.Backend
import Hemmet.Runner

import Hemmet.FileTree.Rendering
import Hemmet.FileTree.Template
import Hemmet.FileTree.Transformation
import Hemmet.FileTree.Tree

type FileTreeBackend = Backend FileTreePayload

type FileTreeRunner = Runner FileTreePayload

fileTree :: FileTreeBackend
fileTree = Backend {getTransformation = get, parser = template}
  where
    get input
        | "|hs|" `isPrefixOf` input = (haskellify, T.drop 4 input)
        | otherwise = (id, input)

bashScript :: FileTreeRunner
bashScript = PureRunner renderBashScript

treeLike :: FileTreeRunner
treeLike = PureRunner renderTreeLike
