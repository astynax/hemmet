{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Data.List
import Lib
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
    args <- getArgs
    let renderer =
            case args of
                ("html":[]) -> Just renderHtml
                ("css":[]) -> Just renderCss
                ("haskell":[]) -> Just renderHaskell
                [] -> Just renderHaskell
                _ -> Nothing
    maybe showHelp run renderer
  where
    showHelp = do
        putStrLn "Usage:\n  hemmet [html|css|haskell]"
        exitWith (ExitFailure 1)
    run render = do
        line <- getLine
        let (pad, preinput) = span (== ' ') line
        let (preprocess, input) =
                case preinput of
                    ('<':xs) -> (stripTop, xs)
                    _ -> (id, preinput)
        case parse template "" input of
            Left err -> do
                putStrLn line -- echo an unchanged line
                hPutStrLn stderr $ show err
                exitWith (ExitFailure 10)
            Right ns -> render pad . preprocess $ transform ns

renderHtml :: String -> [Node] -> IO ()
renderHtml _ [] = return ()
renderHtml pad ((Node name classes _ childs):ns) -- TODO: add vars support
 = do
    putStr $ pad ++ "<" ++ tagName ++ " \"" ++ unwords classes ++ "\">"
    unless (null childs) $ do
        putStrLn ""
        renderHtml (pad ++ "  ") childs
        putStr pad
    putStrLn $ "</" ++ tagName ++ ">"
    renderHtml pad ns
  where
    tagName =
        case name of
            "" -> "div"
            _ -> name

renderHaskell :: String -> [Node] -> IO ()
renderHaskell _ [] = return ()
renderHaskell pad ((Node name classes vars childs):ns) = do
    putStr $ pad ++ tagName ++ " "
    let cs = show . unwords $ classes
    if null vars
        then putStr cs
        else putStr $ "(" ++ intercalate " <> " (cs : vars) ++ ")"
    if null childs
        then putStrLn " $ pure ()"
        else do
            putStrLn " $ do"
            renderHaskell (pad ++ "  ") childs
    renderHaskell pad ns
  where
    tagName =
        case name of
            "" -> "divc_"
            _ -> name

renderCss :: a -> [Node] -> IO ()
renderCss _ = render . sort . collect
  where
    render = mapM_ $ \c -> putStrLn $ '.' : c ++ " {\n}\n"
    collect [] = []
    collect ((Node _ classes _ childs):ns) =
        classes ++ collect childs ++ collect ns

-- preprocessors
type Preprocessor = [Node] -> [Node]

stripTop :: Preprocessor
stripTop [] = []
stripTop (n:_) = _nChilds n
