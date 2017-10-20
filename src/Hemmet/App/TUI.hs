module Hemmet.App.TUI
    ( tui
    ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Control.Monad
import Control.Monad.Trans
import Data.Text
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

data Focus
    = Input
    | Output
    deriving (Eq, Ord, Show)

type State = (Editor Text Focus, Text, Focus)

type Evaluator = Text -> IO Text

tui :: Evaluator -> IO ()
tui run = void $ defaultMain app $ (editorText Input (Just 1) "", "", Input)
  where
    app :: App State Event Focus
    app =
        App
        { appDraw = draw
        , appChooseCursor = chooseCursor
        , appHandleEvent = handle run
        , appStartEvent = return
        , appAttrMap = const . setBorderColor blue $ attrMap defAttr []
        }

draw :: State -> [Widget Focus]
draw (ed, out, focus) = [input <=> output]
  where
    decorate f
        | focus == f = updateAttrMap $ setBorderColor yellow
        | otherwise = id
    input =
        decorate Input $ border $ vLimit 1 $ renderEditor re (focus == Input) ed
    output = decorate Output $ border $ viewport Output Both $ txt out
    re (t:_) = txt t
    re [] = txt ""

handle ::
       Evaluator -> State -> BrickEvent Focus Event -> EventM Focus (Next State)
handle eval s@(ed, _, focus) (VtyEvent e) =
    case e of
        (EvKey KEsc []) -> halt s
        (EvKey (KChar 'c') [MCtrl]) -> halt s
        _
            | focus == Input -> do
                ed' <- handleEditorEvent e ed
                out <- liftIO $ eval $ mconcat $ getEditContents ed'
                continue (ed', out, focus)
        _ -> continue s
handle _ s _ = continue s

setBorderColor :: Color -> AttrMap -> AttrMap
setBorderColor c = applyAttrMappings [(borderAttr, defAttr `withForeColor` c)]

chooseCursor :: State -> [CursorLocation Focus] -> Maybe (CursorLocation Focus)
chooseCursor (_, _, Input) ps =
    case [p | p <- ps, cursorLocationName p == Just Input] of
        (x:_) -> Just x
        _ -> Nothing
chooseCursor _ _ = Nothing
