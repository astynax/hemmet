{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hemmet.Rendering
    ( Renderer
    , renderHtml
    , renderCss
    , renderReactFlux
    ) where

import Control.Monad
import Data.List as L
import Data.Monoid
import Data.Text as T hiding (null)
import qualified Data.Text.IO as TIO

import Hemmet.Tree

type Renderer = Text -> Tree -> IO ()

renderHtml :: Renderer
renderHtml _ [] = return ()
renderHtml pad ((Node name classes _ childs):ns) -- TODO: add vars support
 = do
    TIO.putStr $
        pad <> "<" <> tagName <> " class=\"" <> T.unwords classes <> "\">"
    unless (Prelude.null childs) $ do
        TIO.putStrLn ""
        renderHtml (pad <> "  ") childs
        TIO.putStr pad
    TIO.putStrLn $ "</" <> tagName <> ">"
    renderHtml pad ns
  where
    tagName =
        case name of
            "" -> "div"
            _ -> name

renderReactFlux :: Renderer
renderReactFlux _ [] = return ()
renderReactFlux pad ((Node name classes vars childs):ns) = do
    TIO.putStr $ pad <> tagName <> " "
    let cs = T.unwords $ classes
    if null vars
        then TIO.putStr $ "\"" <> cs <> "\""
        else TIO.putStr $ "( " <> T.intercalate " <> " (cs : vars) <> ")"
    if null childs
        then putStrLn " $ pure ()"
        else do
            TIO.putStrLn " $ do"
            renderReactFlux (pad <> "  ") childs
    renderReactFlux pad ns
  where
    tagName =
        case name of
            "" -> "divc_"
            _
                | "c_" `T.isSuffixOf` name -> name
            _ -> name <> "c_"

renderCss :: Renderer
renderCss _ = render . sort . collect
  where
    render = mapM_ $ \c -> TIO.putStrLn $ cons '.' c <> " {\n}\n"
    collect [] = []
    collect ((Node _ classes _ childs):ns) =
        classes ++ collect childs ++ collect ns
