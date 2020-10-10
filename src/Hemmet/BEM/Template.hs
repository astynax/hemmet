module Hemmet.BEM.Template where

import Data.Char
import Data.Maybe
import Data.Text hiding (map)

import Hemmet.Megaparsec
import Hemmet.Tree

import Hemmet.BEM.Tree

newtype Template =
  Template [Block]
  deriving (Show, Eq)

instance ToTree Template BemPayload where
  toTree = toTree'

data Params =
  Params
  { _pTagName :: Text
  , _pName :: Text
  , _pAddons :: [Addon]
  , _pChilds :: [TemplateNode]
  } deriving (Show, Eq)

type TemplateNode = Either Element Block

newtype Block =
  Block Params
  deriving (Show, Eq)

data Element
  = Element Params
  | ElementBlock
    Text    -- ^ element name
    [Addon] -- ^ element addons
    Params  -- ^ block params
  deriving (Show, Eq)

data Addon
  = Mod Text
  | Var Text
  deriving (Show, Eq)

type Context a = ([Addon], Params -> a, Text)

template :: Parser Template
template = Template <$> many_ alwaysBlock <* eof
  where
    alwaysBlock = templateNode $ (,,) [] Block <$> blockName

templateNode :: Parser (Context a) -> Parser a
templateNode cnn = do
  _pTagName <- try_ identifier
  (prevAddons, ctor, _pName) <- cnn
  _pAddons <- (++) prevAddons <$> many addon
  _pChilds <- try_ childs
  return $ ctor Params {..}
  where
    childs = char '>' *> many_ (templateNode blockOrElement)

blockOrElement :: Parser (Context TemplateNode)
blockOrElement = mkBlock <|> mkElem

mkBlock :: Parser (Context TemplateNode)
mkBlock = (,,) [] (Right . Block) <$> blockName

mkElem :: Parser (Context TemplateNode)
mkElem = do
  _ <- char '.'
  name <- kebabCasedName
  addons <- many addon
  mbBlockName <- optional (char ':' *> kebabCasedName)
  return $ case mbBlockName of
    Just name' -> ([], Left . ElementBlock name addons, name')
    _          -> (addons, Left . Element, name)

blockName :: Parser Text
blockName = char ':' *> kebabCasedName

identifier :: Parser Text
identifier = cons <$> firstChar <*> (pack <$> many restChar)
  where
    firstChar = satisfy isAsciiLower <|> satisfy isAsciiUpper <|> char '_'
    restChar = firstChar <|> digitChar

addon :: Parser Addon
addon = mod_ <|> var
  where
    mod_ = Mod <$> (char '~' *> modName)
    var = Var <$> (char '$' *> identifier)

kebabCasedName :: Parser Text
kebabCasedName =
  cons <$> lascii <*> (pack <$> many (char '-' <|> lascii <|> digitChar))
  where
    lascii = satisfy isAsciiLower

modName :: Parser Text
modName = (<>) <$> kebabCasedName <*> possibleValue
  where
    possibleValue = try_ $ cons <$> char '_' <*> kebabCasedName

many_ :: Parser a -> Parser [a]
many_ p = between (char '(') (char ')') ps <|> ps
  where
    ps = p `sepBy` char '+'

try_ :: Monoid m => Parser m -> Parser m
try_ = (<|> pure mempty)

-- transrormation to Tree
toTree' :: Template -> Tree BemPayload
toTree' (Template bs) = BemPayload [] [] $ map (transformBlock "") bs

transformBlock :: Text -> Block -> Node BemPayload
transformBlock _ (Block p) = transform' (flip const) (_pName p) p

transformElement :: Text -> Element -> Node BemPayload
transformElement parent (Element p)                              =
  transform' (prefix "__") parent p
transformElement parent (ElementBlock elName elAddons blPayload) =
  let
    n = transformBlock parent (Block blPayload)
    (Node _ (BemPayload cs vs _)) =
      transformElement parent $ Element $ Params "" elName elAddons []
    pl = _nPayload n
    newPayload =
      pl {_bpClasses = _bpClasses pl ++ cs, _bpVars = _bpVars pl ++ vs}
  in n {_nPayload = newPayload}

transform' :: (Text -> Text -> Text) -> Text -> Params -> Node BemPayload
transform' use parent (Params _nName name addons childs) = Node {..}
  where
    n = use parent name
    _nPayload = BemPayload {..}
    _bpClasses =
      (n :) $
      flip mapMaybe addons $ \case
        Var _ -> Nothing
        Mod m -> Just $ prefix "_" n m
    _bpVars =
      flip mapMaybe addons $ \case
        Var v -> Just v
        _     -> Nothing
    _bpChilds =
      map (either (transformElement parent) (transformBlock parent)) childs

prefix :: Text -> Text -> Text -> Text
prefix sep p n = p <> sep <> n
