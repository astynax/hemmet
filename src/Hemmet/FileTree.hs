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
fileTree =
    Backend
    {getTransformation = get, parser = template, examples = fileTreeExamples}
  where
    get input
        | "|hs|" `isPrefixOf` input = (haskellify, T.drop 4 input)
        | "|py|" `isPrefixOf` input = (pythonify, T.drop 4 input)
        | otherwise = (id, input)

bashScript :: FileTreeRunner
bashScript = PureRunner renderBashScript

treeLike :: FileTreeRunner
treeLike = PureRunner renderTreeLike

fileTreeExamples :: [(Text, Text)]
fileTreeExamples =
    [ ("simple", "file")
    , ( "complex"
      , "file1 folderA/{fileA1 folderAA/ folderAB/{fileAB1 fileAB2} fileA2}\
        \ folderB/folderBA/folderBAA/fileBAA1 file2")
    , ("ordering", "c aa b cc a bb")
    , ("transformation: Haskell project", "|hs|app/main src/!lib/{types utils}")
    ]
