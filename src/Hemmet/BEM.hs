module Hemmet.BEM
  ( BemBackend
  , BemRunner
  , bem
  , bemHtml
  , bemCss
  , bemReactFlux
  ) where

import Data.Text as T

import Hemmet.Backend
import Hemmet.Runner

import Hemmet.BEM.Rendering
import Hemmet.BEM.Template
import Hemmet.BEM.Transformation
import Hemmet.BEM.Tree

type BemBackend = Backend BemPayload

type BemRunner = Runner BemPayload

bem :: BemBackend
bem =
  Backend
    { getTransformation = \input ->
       if "<" `T.isPrefixOf` input
       then (stripTopNode, T.tail input)
       else (id, input)
    , parser = template
    , examples = bemExamples
    }

bemHtml :: BemRunner
bemHtml = PureRunner renderHtmlM

bemCss :: BemRunner
bemCss = PureRunner renderCssM

bemReactFlux :: BemRunner
bemReactFlux = PureRunner renderReactFluxM

bemExamples :: [(Text, Text)]
bemExamples =
  [ ("minimal", ":foo")
  , ( "complex"
    , "form:search-form$theme>input.query>(div.help~hidden_t)+\
      \span.submit-button~disabled_t:button~text_small>.hint"
    )
  , ("transformation: top node strip", "<:block>.elem")
  ]
