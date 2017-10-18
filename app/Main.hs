{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Text (pack)
import qualified Data.Text.IO as T
import Options.Applicative
import System.Exit
import System.IO

import Hemmet hiding (Parser)

data Input
    = Expression String
    | Examples
    | StdIn

data Options where
    Options :: forall a. Backend a -> Runner a -> Input -> Options

main :: IO ()
main = do
    (Options backend runner input) <- execParser cli
    let run' = run backend runner
    case input of
        StdIn -> T.getLine >>= run'
        Expression e -> run' $ pack e
        Examples ->
            forM_ (examples backend) $ \(desc, inp) -> do
                T.putStrLn $ "Example \"" <> desc <> "\""
                T.putStrLn "<<<"
                T.putStrLn inp
                T.putStrLn ">>>"
                run' inp
                T.putStrLn ""
                T.putStrLn ""
  where
    run backend runner input =
        case runHemmet backend runner input of
            Left err -> do
                T.putStr input -- echo an unchanged line
                hPutStr stderr $ parseErrorPretty' input err
                exitWith (ExitFailure 10)
            Right res ->
                case res of
                    Pure t -> T.putStr t
                    Effect e -> e

-- options
cli :: ParserInfo Options
cli =
    info
        (commands <**> helper)
        (progDesc "Expands the snippets" <>
         header "Hemmet, the snippet expander" <>
         fullDesc)

commands :: Parser Options
commands =
    subparser $
    mkCommand "bem" "Generates BEM markup" bem argBemRunner <>
    mkCommand "ftree" "Generates the file trees" fileTree argFileTreeRunner
  where
    mkCommand cmd desc backend argRunner =
        command cmd $
        info (Options backend <$> argRunner <*> optInput <**> helper) $
        progDesc desc

argBemRunner :: Parser BemRunner
argBemRunner = fromMaybe bemReactFlux <$> optional arg'
  where
    arg' = argument reader $ metavar "html|css|react-flux"
    reader =
        eitherReader $ \raw ->
            case raw of
                "" -> Right bemReactFlux
                "react-flux" -> Right bemReactFlux
                "html" -> Right bemHtml
                "css" -> Right bemCss
                _ -> Left $ "Unknown renderer: " ++ raw

argFileTreeRunner :: Parser FileTreeRunner
argFileTreeRunner = fromMaybe treeLike <$> optional arg'
  where
    arg' = argument reader $ metavar "tree|bash"
    reader =
        eitherReader $ \raw ->
            case raw of
                "" -> Right treeLike
                "tree" -> Right treeLike
                "bash" -> Right bashScript
                _ -> Left $ "Unknown renderer: " ++ raw

optInput :: Parser Input
optInput = fromMaybe StdIn <$> (optional example <|> optional expression)
  where
    expression =
        Expression <$>
        strOption
            (short 'e' <> long "expression" <> metavar "EXPRESSION" <>
             help "Expression (snippet) to expand")
    example = flag' Examples (long "examples" <> help "Show some examples")
