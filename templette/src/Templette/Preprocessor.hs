{-# LANGUAGE OverloadedStrings #-}

module Templette.Preprocessor (
  preprocessWith,
) where

import qualified Data.Set as Set
import Data.Text (Text)

import Templette.Config (TempletteConfig)
import Templette.Preprocessor.Parse

import Debug.Trace

preprocessWith :: TempletteConfig -> FilePath -> Text -> Either Text Text
preprocessWith _ fp t = (\x -> traceShow x undefined) <$> parseTemplette options fp t
  where
    options =
      -- TODO: from config
      TempletteParseOptions
        { delimStart = '{'
        , delimEnd = '}'
        , directiveStart = '$'
        , rawDirectives = Set.fromList ["setup"]
        }
