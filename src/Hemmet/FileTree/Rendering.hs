module Hemmet.FileTree.Rendering
  ( renderBashScript
  , renderTreeLike
  ) where

import Control.Monad
import Data.Function
import Data.List ( sortBy )

import Hemmet.Rendering
import Hemmet.Tree

import Hemmet.FileTree.Tree

renderBashScript :: Renderer FileTreePayload
renderBashScript File           = pure ()
renderBashScript (Directory xs) = do
  out "#!/bin/bash" >> nl
  addPreview
  out "read -p \"Press any key to continue...\" -n1 -s" >> nl
  out "set -euf -o pipefail" >> nl
  forM_ (sorted xs) render
  where
    addPreview = do
      out "cat <<PREVIEW_END" >> nl
      out "This file tree will be created:" >> nl
      renderTreeLike (Directory xs)
      out "PREVIEW_END" >> nl
    render (Node p File) = pad >> cmd "touch" p >> nl
    render (Node p (Directory ns)) = do
        pad >> cmd "mkdir" p
        if null ns
            then nl
            else do
                cmd " && pushd" p >> nl
                forM_ (sorted ns) $ withOffset 2 . render
                pad >> out "popd" >> nl
    cmd c p = out (c <> " \"" <> p <> "\"")

renderTreeLike :: Renderer FileTreePayload
renderTreeLike File           = pure ()
renderTreeLike (Directory xs) = do
  out "."
  nl
  render (sorted xs)
  where
    render [] = pure ()
    render (Node p File:ns) = outLn ns p >> render ns
    render (Node p (Directory ns):nns) = do
      outLn nns (p <> "/")
      let
        padding =
          if null nns
          then "    "
          else "│   "
      paddedWith padding $ render (sorted ns)
      render nns
    outLn ns p = pad >> out (branch ns) >> out p >> nl
    branch [] = "└── "
    branch _  = "├── "

sorted :: [Node a] -> [Node a]
sorted = sortBy (compare `on` _nName)
