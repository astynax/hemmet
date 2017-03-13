{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hemmet.Rendering
    ( Renderer
    , RendererM
    , runRenderM
    , renderHtmlM
    , renderCssM
    , renderReactFluxM
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List as L
import Data.Text as T hiding (null)

import Hemmet.Tree

type RendererM = ReaderT Int (Writer Text) ()

type Renderer = Tree -> RendererM

runRenderM :: RendererM -> Int -> Text
runRenderM renderer offset = execWriter $ runReaderT renderer offset

pad :: RendererM
pad = ask >>= \n -> lift (tell $ T.replicate n " ")

nl :: RendererM
nl = lift $ tell "\n"

out :: Text -> RendererM
out = lift . tell

withOffset :: Int -> RendererM -> RendererM
withOffset n = withReaderT (+ n)

renderHtmlM :: Renderer
renderHtmlM [] = pure ()
renderHtmlM ((Node name classes _ childs):ns) = do
    pad
    out $ "<" <> tagName <> " class=\"" <> T.unwords classes <> "\">"
    unless (Prelude.null childs) $ do
        nl
        withOffset 2 $ renderHtmlM childs
        pad
    out $ "</" <> tagName <> ">"
    nl
    renderHtmlM ns
  where
    tagName =
        case name of
            "" -> "div"
            _ -> name

renderReactFluxM :: Renderer
renderReactFluxM [] = pure ()
renderReactFluxM ((Node name classes vars childs):ns) = do
    pad
    out $ tagName <> " "
    let cs = T.unwords $ classes
    out $
        if null vars
            then "\"" <> cs <> "\""
            else "( " <> T.intercalate " <> " (cs : vars) <> ")"
    if null childs
        then out " $ pure ()" >> nl
        else do
            out " $ do" >> nl
            withOffset 2 $ renderReactFluxM childs
    renderReactFluxM ns
  where
    tagName =
        case name of
            "" -> "divc_"
            _
                | "c_" `T.isSuffixOf` name -> name
            _ -> name <> "c_"

renderCssM :: Renderer
renderCssM = render . sort . collect
  where
    render =
        mapM_ $ \c -> do
            pad
            out $ cons '.' c <> " {"
            nl
            pad
            out "}"
            nl
            nl
    collect [] = []
    collect ((Node _ classes _ childs):ns) =
        classes ++ collect childs ++ collect ns
