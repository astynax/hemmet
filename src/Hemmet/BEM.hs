{-# LANGUAGE OverloadedStrings #-}

module Hemmet.BEM
    ( module R
    , BemPayload
    , bem
    ) where

import Data.Text as T

import Hemmet.Backend

import Hemmet.BEM.Rendering as R
import Hemmet.BEM.Template
import Hemmet.BEM.Transformation
import Hemmet.BEM.Tree

bem :: Backend BemPayload
bem =
    Backend
    { getTransformation =
          \input ->
              if "<" `T.isPrefixOf` input
                  then (stripTopNode, T.tail input)
                  else (id, input)
    , parser = template
    }
