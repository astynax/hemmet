{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Data.Maybe
import Data.Text (pack)
import Options.Applicative
import System.Exit
import System.IO

import Hemmet

data Options = Options
    { renderer :: Renderer
    , input :: IO String
    }

main :: IO ()
main = configure >>= run
  where
    configure = execParser cli
    run opts = do
        line <- input opts
        let (pad, preinput) = span (== ' ') line
        let (preprocess, datum) =
                if "<" `isPrefixOf` preinput
                    then (stripTop, tail preinput)
                    else (id, preinput)
        case parse template "" (pack datum) of
            Left err -> do
                putStr line -- echo an unchanged line
                hPutStrLn stderr $ show err
                exitWith (ExitFailure 10)
            Right ns -> renderer opts (pack pad) . preprocess $ toTree ns

-- options
cli :: ParserInfo Options
cli =
    info
        (options <**> helper)
        (progDesc "Expands the template string" <>
         header "Hemmet, the snippet expander" <>
         fullDesc)

options :: Parser Options
options = Options <$> renderTo <*> inputFrom

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

inputFrom :: Parser (IO String)
inputFrom = maybe getLine pure <$> optional opt'
  where
    opt' =
        strOption
            (short 'e' <> long "expression" <> metavar "EXPRESSION" <>
             help "Expression (snippet) to expand")

-- transformations
stripTop :: Transformation
stripTop [] = []
stripTop (n:_) = _nChilds n
