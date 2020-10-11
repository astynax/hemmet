module Hemmet.Dom.Rendering
  ( renderHtmlM
  , renderCssM
  ) where

import Control.Monad
import Data.Foldable
import Data.List as L
import Data.Text as T

import Hemmet.Rendering
import Hemmet.Tree

import Hemmet.Dom.Tree

type NodeRenderer = Node DomPayload -> RendererM

renderHtmlM :: Renderer DomPayload
renderHtmlM = run renderHtmlM'

renderHtmlM' :: NodeRenderer
renderHtmlM' (Node name (DomPayload mbId classes childs)) = do
  pad
  out $ mconcat $ ["<", tagName] <> tagId <> tagClasses <> [">"]
  unless (Prelude.null childs) $ do
    nl
    withOffset 2 $ traverse_ renderHtmlM' childs
    pad
  out $ "</" <> tagName <> ">"
  nl
  where
    tagName =
      case name of
        "" -> "div"
        _  -> name
    tagId =
      case mbId of
        Just x  -> [" id=\"", x, "\""]
        Nothing -> []
    tagClasses
      | Prelude.null classes = []
      | otherwise            = [" class=\"", T.unwords classes, "\""]

renderCssM :: Renderer DomPayload
renderCssM = run renderCssM'

renderCssM' :: NodeRenderer
renderCssM' = render . annotate . sort . collect
  where
    render = mapM_ $ \(cls, isLast) -> do
      pad
      out $ cons '.' cls <> " {"
      nl
      pad
      out "}"
      nl
      unless isLast nl
    annotate [] = []
    annotate xs@(_:rest) = L.zip xs $ L.map (const False) rest ++ [True]
    collect (Node _ (DomPayload _ classes childs)) =
      L.nub $ classes ++ L.concatMap collect childs

run :: NodeRenderer -> Renderer DomPayload
run r = traverse_ r . _dpChilds
