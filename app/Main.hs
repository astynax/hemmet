{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Data.Maybe
import Data.Text (pack)
import Options.Applicative
import System.Exit
import System.IO

import Hemmet

type Options = Renderer

main :: IO ()
main = configure >>= run
  where
    configure = execParser options
    run render = do
        line <- getLine
        let (pad, preinput) = span (== ' ') line
        let (preprocess, input) =
                if "<" `isPrefixOf` preinput
                    then (stripTop, tail preinput)
                    else (id, preinput)
        case parse template "" (pack input) of
            Left err -> do
                putStr line -- echo an unchanged line
                hPutStrLn stderr $ show err
                exitWith (ExitFailure 10)
            Right ns -> render (pack pad) . preprocess $ toTree ns

-- options
options :: ParserInfo Options
options =
    info
        (renderTo <**> helper)
        (progDesc "Expands the template string" <>
         header "Hemmet, the snippet expander" <>
         fullDesc)

renderTo :: Parser Renderer
renderTo = fromMaybe renderReactFlux <$> optional arg'
  where
    arg' = argument reader $ metavar "html|css|react-flux"
    reader =
        eitherReader $ \raw ->
            case raw of
                "" -> Right renderReactFlux
                "react-flux" -> Right renderReactFlux
                "html" -> Right renderHtml
                "css" -> Right renderCss
                _ -> Left $ "Unknown renderer: " ++ raw

-- transformations
stripTop :: Transformation
stripTop [] = []
stripTop (n:_) = _nChilds n
