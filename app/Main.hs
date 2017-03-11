{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text as T hiding (null)
import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit
import System.IO

import Hemmet

main :: IO ()
main = do
    args <- getArgs
    let renderer =
            case args of
                ("html":[]) -> Just renderHtml
                ("css":[]) -> Just renderCss
                ("react-flux":[]) -> Just renderReactFlux
                [] -> Just renderReactFlux
                _ -> Nothing
    maybe showHelp run renderer
  where
    showHelp = do
        putStrLn "Usage:\n  hemmet [html|css|react-flux]"
        exitWith (ExitFailure 1)
    run render = do
        line <- pack <$> getLine
        let (pad, preinput) = T.span (== ' ') line
        let (preprocess, input) =
                if "<" `T.isPrefixOf` preinput
                    then (stripTop, T.tail preinput)
                    else (id, preinput)
        case parse template "" input of
            Left err -> do
                TIO.putStr line -- echo an unchanged line
                hPutStrLn stderr $ show err
                exitWith (ExitFailure 10)
            Right ns -> render pad . preprocess $ toTree ns

-- transformations
stripTop :: Transformation
stripTop [] = []
stripTop (n:_) = _nChilds n
