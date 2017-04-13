module Hemmet.FileTree
    ( FileTreeBackend
    , FileTreeRunner
    , fileTree
    , shellScript
    , treeLike
    ) where

import Hemmet.Backend
import Hemmet.Runner

import Hemmet.FileTree.Rendering
import Hemmet.FileTree.Template
import Hemmet.FileTree.Tree

type FileTreeBackend = Backend FileTreePayload

type FileTreeRunner = Runner FileTreePayload

fileTree :: FileTreeBackend
fileTree =
    Backend {getTransformation = \input -> (id, input), parser = template}

shellScript :: FileTreeRunner
shellScript = PureRunner renderShellScript

treeLike :: FileTreeRunner
treeLike = PureRunner renderTreeLike
