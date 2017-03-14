{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Data.Text (pack)
import Data.Text.IO as TIO (putStr)
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
        case runHemmet (renderer opts) (pack line) of
            Left err -> do
                Prelude.putStr line -- echo an unchanged line
                hPutStrLn stderr $ show err
                exitWith (ExitFailure 10)
            Right res -> TIO.putStr res

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
renderTo = fromMaybe renderReactFluxM <$> optional arg'
  where
    arg' = argument reader $ metavar "html|css|react-flux"
    reader =
        eitherReader $ \raw ->
            case raw of
                "" -> Right renderReactFluxM
                "react-flux" -> Right renderReactFluxM
                "html" -> Right renderHtmlM
                "css" -> Right renderCssM
                _ -> Left $ "Unknown renderer: " ++ raw

inputFrom :: Parser (IO String)
inputFrom = maybe getLine pure <$> optional opt'
  where
    opt' =
        strOption
            (short 'e' <> long "expression" <> metavar "EXPRESSION" <>
             help "Expression (snippet) to expand")
