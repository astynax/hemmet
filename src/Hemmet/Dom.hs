module Hemmet.Dom
  ( DomBackend
  , DomRunner
  , dom
  , domHtml
  , domCss
  ) where

import Data.Text as T

import Hemmet.Backend
import Hemmet.Runner

import Hemmet.Dom.Rendering
import Hemmet.Dom.Template
import Hemmet.Dom.Tree

type DomBackend = Backend DomPayload

type DomRunner = Runner DomPayload

dom :: DomBackend
dom =
  Backend
    { getTransformation = (id,)
    , parser = template
    , examples = domExamples
    }

domHtml :: DomRunner
domHtml = PureRunner renderHtmlM

domCss :: DomRunner
domCss = PureRunner renderCssM

domExamples :: [(Text, Text)]
domExamples =
  [ ( "Simple", "html>head+body>h1.red+p#article" )
  , ( "Typical"
    , "#root>.nav>(ul.menu>li.item+li.item)+#content+.footer>img.logo"
    )
  ]
