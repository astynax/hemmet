module Hemmet.FileTree.Transformation where

import Data.Text as T hiding (concatMap, map)

import Hemmet.Tree

import Hemmet.FileTree.Tree

haskellify :: Transformation FileTreePayload
haskellify File = File
haskellify (Directory nodes) = Directory $ concatMap process nodes
  where
    process (Node name File) = [Node (haskellifyFile name) File]
    process (Node name d)
      | "!" `isPrefixOf` name =
        let n = T.tail name
        in Node (haskellifyFile n) File : process (Node n d)
    process (Node name (Directory ns)) =
      [Node (haskellifyDir name) . Directory $ concatMap process ns]
    haskellifyFile = (<> ".hs") . camelCase
    haskellifyDir name
      | name `elem` ["src", "test"] = name
      | otherwise = camelCase name
    camelCase = T.concat . map toTitle . split (== '-')

pythonify :: Transformation FileTreePayload
pythonify File = File
pythonify (Directory nodes) = Directory $ concatMap process nodes
  where
    process (Node name File) = [Node (pythonifyFile name) File]
    process (Node name (Directory ns)) =
      let
        initFile
          | name `elem` ["src", "test"] = []
          | otherwise = [Node "__init__.py" File]
      in [Node (snakeCase name) . Directory $ initFile ++ concatMap process ns]
    pythonifyFile = (<> ".py") . snakeCase
    snakeCase = T.intercalate "_" . split (== '-')
