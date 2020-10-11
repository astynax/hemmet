module Hemmet.Dom.Rendering
  ( renderHtmlM
  , renderCssM
  , renderElmM
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
  out ("</" <> tagName <> ">") >> nl
  where
    tagName =
      case name of
        "" -> "div"
        _  -> name
    tagId =
      case mbId of
        Just x  -> [" id=\"", x, "\""]
        Nothing -> []
    tagClasses =
      case classes of
        [] -> []
        _  -> [" class=\"", T.unwords classes, "\""]

renderCssM :: Renderer DomPayload
renderCssM = run renderCssM'

renderCssM' :: NodeRenderer
renderCssM' = render . annotate . sort . collect
  where
    render = mapM_ $ \(cls, isLast) -> do
      pad >> out ("." <> cls <> " {") >> nl
      pad >> out "}" >> nl
      unless isLast nl
    annotate [] = []
    annotate xs@(_:rest) = L.zip xs $ L.map (const False) rest ++ [True]
    collect (Node _ (DomPayload _ classes childs)) =
      L.nub $ classes ++ L.concatMap collect childs

renderElmM :: Renderer DomPayload
renderElmM = run $ renderElmM' pad

renderElmM' :: RendererM -> NodeRenderer
renderElmM' fstPad (Node name (DomPayload mbId classes childs)) = do
  fstPad >> out (tagName <> " " <> tagAttrs)
  case childs of
    []     -> out " []" >> nl
    (c:cs) -> do
      nl
      withOffset 4 $ do
        renderElmM' (pad >> out "[ ") c
        traverse_ (renderElmM' $ pad >> out ", ") cs
        pad >> out "]" >> nl
  where
    tagName =
      case name of
        "" -> "div"
        _  -> name
    tagId =
      case mbId of
        Just x  -> ["id \"" <> x <> "\""]
        Nothing -> []
    tagClasses = L.map (\c -> "class \"" <> c <> "\"") classes
    tagAttrs = case tagId <> tagClasses of
      [] -> "[]"
      as -> "[ " <> T.intercalate ", " as <> " ]"

run :: NodeRenderer -> Renderer DomPayload
run r = traverse_ r . _dpChilds
