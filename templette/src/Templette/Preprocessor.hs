{-# LANGUAGE OverloadedStrings #-}

module Templette.Preprocessor (
  preprocessWith,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)

import Templette.Config (TempletteConfig)
import Templette.Preprocessor.Directives
import Templette.Preprocessor.Parse
import Templette.Preprocessor.Transform

preprocessWith :: (Monad m) => TempletteConfig m -> FilePath -> Text -> m (Either Text Text)
preprocessWith _ fp t =
  case parseTemplette parseOptions fp t of
    Right input -> transformWith transformOptions input
    Left e -> pure $ Left e
  where
    parseOptions =
      -- TODO: from config
      TempletteParseOptions
        { delimStart = '{'
        , delimEnd = '}'
        , directiveStart = '$'
        , rawDirectives = Set.fromList ["setup"]
        }
    transformOptions =
      -- TODO: from config
      TempletteTransformOptions
        { templettePreprocess = pure . Right
        , templetteDirectives =
            Map.fromList
              [ ("setup", setupDirective)
              , ("define", defineDirective)
              , ("call", callDirective)
              ]
        , templettePostprocess = pure . Right
        }
