{-# LANGUAGE RecordWildCards #-}

module Templette.Preprocessor (
  preprocessWith,
) where

import qualified Data.Map as Map
import Data.Text (Text)
import Debug.Trace

import Templette.Config
import Templette.Preprocessor.Parse
import Templette.Preprocessor.Transform

preprocessWith :: (Monad m) => TempletteConfig m -> FilePath -> Text -> m (Either Text Text)
preprocessWith TempletteConfig{..} fp t =
  case parseTemplette parseOptions fp t of
    Right input -> traceShow input $ transformWith transformOptions input
    Left e -> pure $ Left e
  where
    parseOptions =
      TempletteParseOptions
        { delimStart = cfgDelimStart
        , delimEnd = cfgDelimEnd
        , directiveStart = cfgDirectiveStart
        , rawDirectives = Map.keysSet $ Map.filter cfgDirectiveParseRaw cfgDirectives
        }
    transformOptions =
      TempletteTransformOptions
        { templettePreprocess = cfgPreprocess
        , templetteDirectives = cfgDirectiveRun <$> cfgDirectives
        , templettePostprocess = cfgPostprocess
        , templetteImports = cfgImplicitImports
        }
