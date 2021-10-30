{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}

module Hemmet.Cli
  ( Options(..)
  , Input(..)
  , Output(..)
  , cliWith
  , optInput
  , optOutput
  ) where

import Data.Kind (Type)
import Data.Maybe
import Options.Applicative

import Hemmet hiding (Parser)

data Input
  = Expression String
  | Examples
  | StdIn

data Output
  = File FilePath
  | StdOut

data Options :: Type -> Type where
  Options :: forall a e. e -> Runner a -> Backend a -> Options e

cliWith :: Parser a -> ParserInfo (Options a)
cliWith extra =
  info
    (commands extra <**> helper)
    (progDesc "Expands the snippets" <>
      header "Hemmet, the snippet expander" <>
      fullDesc)

commands :: Parser a -> Parser (Options a)
commands extra = subparser $ bemC <> domC <> ftreeC
  where
    bemC = cmd "bem" (bem <$$> argBemRunner extra) "Generates BEM markup"
    domC = cmd "dom" (dom <$$> argDomRunner extra) "Generates DOM markup"
    ftreeC =
      cmd "ftree"
        (fileTree <$$> argFileTreeRunner extra)
        "Generates the file trees"

argBemRunner :: Parser a -> Parser (BemBackend -> Options a)
argBemRunner extra = subparser $ mconcat
  [ outCmd extra "react-flux" bemReactFlux "react-flux code"
  , outCmd extra "html" bemHtml "HTML"
  , outCmd extra "css" bemCss "CSS"
  ]

argDomRunner :: Parser a -> Parser (DomBackend -> Options a)
argDomRunner extra = subparser $ mconcat
  [ outCmd extra "html" domHtml "HTML"
  , outCmd extra "lucid" domLucid "Lucid HTML"
  , outCmd extra "css" domCss "CSS"
  , outCmd extra "elm" domElm "Elm.Html"
  , outCmd extra "hamlet" domHamlet "Shakespeare/Hamlet"
  , outCmd extra "cassius" domCassius "Shakespeare/Cassius"
  , outCmd extra "ktxhtml" domKotlinxHtml "Kotlinx.Html"
  ]

argFileTreeRunner :: Parser a -> Parser (FileTreeBackend -> Options a)
argFileTreeRunner extra = subparser $ mconcat
  [ outCmd extra "tree" treeLike "GNU Tree-like output"
  , outCmd extra "bash" bashScript "Bash-script"
  ]

optInput :: Parser Input
optInput = fromMaybe StdIn <$> (optional example <|> optional expression)
  where
    example = flag' Examples (long "examples" <> help "Show some examples")
    expression =
      Expression <$> strOption
        (short 'e' <> long "expression" <> metavar "EXPRESSION" <>
           help "Expression (snippet) to expand")

optOutput :: Parser Output
optOutput = fromMaybe StdOut <$> optional fileName
  where
    fileName =
      File <$> strOption
        (short 'O' <> long "out-file" <> metavar "FILE" <>
           help "Output file")

cmd :: String -> Parser a -> String -> Mod CommandFields a
cmd c p desc = command c $ info (p <**> helper) $ progDesc desc

outCmd extra c d n = cmd c (d <$$> fmap Options extra) $ "Generates " <> n

(<$$>) :: Functor f => a -> f (a -> b) -> f b
x <$$> f = ($ x) <$> f
