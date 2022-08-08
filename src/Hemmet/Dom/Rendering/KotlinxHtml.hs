module Hemmet.Dom.Rendering.KotlinxHtml where

import Control.Monad
import Data.Foldable
import qualified Data.List as L

import Hemmet.Rendering
import Hemmet.Tree

import Hemmet.Dom.Rendering.Common
import Hemmet.Dom.Tree

renderKotlinxHtmlM :: Renderer DomPayload
renderKotlinxHtmlM = run render
  where
    render (Node name (DomTag mbId classes childs)) = do
      let tagName = if name == "" then "div" else name
      pad
      out $ tagName <> " {"
      case (mbId, classes, childs) of
        (Nothing, [], []) -> pure ()
        _                 -> do
          nl
          withOffset 4 $ do
            case mbId of
              Just x -> do
                pad
                out $"id = \"" <> x <> "\""
                nl
              _      -> pure ()
            unless (L.null classes) $ do
              pad
              out $ "classes = setOf("
                <> mconcat (L.intersperse ", " $ L.map quoted classes)
                <> ")"
              nl
            unless (L.null childs) $ do
              traverse_ render childs
          pad
      out "}"
      nl
    render (Node _ (DomPlainText text)) = do
      pad
      out $ "+\"" <> text <> "\""
      nl