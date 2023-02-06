{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Templette.Config (
  TempletteConfig (..),
  defaultConfig,
  TempletteDirective (..),
  defaultDirectives,
  TempletteM,
  TempletteState (..),
  -- runTempletteM,
) where

import Control.Monad.State (State, runState)
import Data.Text (Text)

{- TODO:
TempletteOutput = TempletteOutputRaw | TempletteOutputCode

preprocess :: [TempletteInput] -> m [TempletteInput]
  = pure
directives :: [(Text, TempletteInputDirective -> m [TempletteOutput])]
  = [...]
renderOutput :: [(Text, Text)] -> Text -> m Text
  = \inputs output -> "templetteOutput :: <inputs>\ntempletteOutput <inputs> = <output>"
postprocess :: [TempletteOutput] -> m [TempletteOutput]
  = pure
-}
data TempletteConfig = TempletteConfig

defaultConfig :: TempletteConfig
defaultConfig = TempletteConfig

data TempletteDirective = TempletteDirective
  { directiveName :: Text
  , directiveImpl :: [Text] -> Text -> TempletteM ()
  }

defaultDirectives :: [TempletteDirective]
defaultDirectives = []

newtype TempletteM a = TempletteM {unTempletteM :: State TempletteState a}
  deriving (Functor, Applicative, Monad)

data TempletteState = TempletteState
  -- {
  -- }

-- runTempletteM :: TempletteM a -> (a, (Text, Text))
-- runTempletteM

