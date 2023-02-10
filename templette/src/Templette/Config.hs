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

data TempletteConfig m = TempletteConfig
  { _foo :: m ()
  }

defaultConfig :: (Monad m) => TempletteConfig m
defaultConfig =
  TempletteConfig
    { _foo = pure ()
    }

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

