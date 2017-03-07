module Main where

import Control.Monad
import Data.List
import Lib
import System.Exit
import System.IO

main :: IO ()
main = do
    line <- getLine
    let (pad, preinput) = span (== ' ') line
    let (preprocess, input) =
            case preinput of
                ('<':xs) -> (stripTop, xs)
                _ -> (id, preinput)
    case parse template "" input of
        Left err -> do
            hPutStrLn stdout $ show err
            exitWith (ExitFailure 1)
        Right ns -> renderHaskell pad . preprocess $ transform ns

renderHtml :: String -> [Node] -> IO ()
renderHtml _ [] = return ()
renderHtml pad ((Node name classes elems):ns) = do
    putStr $ pad ++ "<" ++ tagName ++ " \"" ++ unwords classes ++ "\">"
    unless (null elems) $ do
        putStrLn ""
        renderHtml (pad ++ "  ") elems
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
renderHaskell pad ((Node name classes elems):ns) = do
    putStr $ pad ++ tagName ++ " "
    let cs = map show classes
    case cs of
        [c] -> putStr c
        _ -> putStr $ "(" ++ intercalate " <> " cs ++ ")"
    case elems of
        [] -> putStrLn " $ pure ()"
        _ -> do
            putStrLn " $ do"
            renderHaskell (pad ++ "  ") elems
    renderHaskell pad ns
  where
    tagName =
        case name of
            "" -> "divc_"
            _ -> name

-- preprocessors
type Preprocessor = [Node] -> [Node]

stripTop :: Preprocessor
stripTop [] = []
stripTop ((Node _ _ es):_) = es
