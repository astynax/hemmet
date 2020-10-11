module Hemmet.BEM.Rendering
  ( renderHtmlM
  , renderCssM
  , renderReactFluxM
  ) where

import Control.Monad
import Data.Foldable
import Data.List as L
import Data.Text as T

import Hemmet.Rendering
import Hemmet.Tree

import Hemmet.BEM.Tree

type NodeRenderer = Node BemPayload -> RendererM

renderHtmlM :: Renderer BemPayload
renderHtmlM = run renderHtmlM'

renderCssM :: Renderer BemPayload
renderCssM = run renderCssM'

renderReactFluxM :: Renderer BemPayload
renderReactFluxM = run renderReactFluxM'

renderHtmlM' :: NodeRenderer
renderHtmlM' (Node name (BemPayload classes _ childs)) = do
  pad
  out $ "<" <> tagName <> " class=\"" <> T.unwords classes <> "\">"
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

renderReactFluxM' :: NodeRenderer
renderReactFluxM' (Node name (BemPayload classes vars childs)) = do
  pad
  out $ tagName <> " "
  out $ case vars of
    [] -> tagClasses
    _  -> "(" <> T.intercalate " <> " (tagClasses : vars) <> ")"
  case childs of
    [] -> out " $ pure ()" >> nl
    _  -> do
      out " $ do" >> nl
      withOffset 2 $ traverse_ renderReactFluxM' childs
  where
    tagName =
      case name of
        ""                          -> "divc_"
        _ | "_" `T.isSuffixOf` name -> name
        _                           -> name <> "c_"
    tagClasses =
      "\"" <> T.unwords classes <> "\""

renderCssM' :: NodeRenderer
renderCssM' = render . annotate . sort . collect
  where
    render = mapM_ $ \(cls, isLast) -> do
      pad >> out ("." <> cls <> " {") >> nl
      pad >> out "}" >> nl
      unless isLast nl
    annotate [] = []
    annotate xs@(_:rest) = L.zip xs $ L.map (const False) rest ++ [True]
    collect (Node _ (BemPayload classes _ childs)) =
      classes ++ L.concatMap collect childs

run :: NodeRenderer -> Renderer BemPayload
run r = traverse_ r . _bpChilds
