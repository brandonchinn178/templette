{-# LANGUAGE OverloadedStrings #-}

module Templette.Config (
  TempletteConfig (..),
  DirectiveInfo (..),
  defaultConfig,
  defaultDirectives,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import qualified Templette.Directives as Directives
import Templette.Preprocessor.Parse (TempletteDirective, TempletteInput)
import Templette.Preprocessor.Transform (TempletteOutput)

data TempletteConfig m = TempletteConfig
  { cfgDelimStart :: Char
  , cfgDelimEnd :: Char
  , cfgDirectiveStart :: Char
  , cfgDirectives :: Map Text (DirectiveInfo m)
  , cfgPreprocess :: [TempletteInput] -> m (Either Text [TempletteInput])
  , cfgPostprocess :: [TempletteOutput] -> m (Either Text [TempletteOutput])
  , cfgImplicitImports :: [Text]
  }

data DirectiveInfo m = DirectiveInfo
  { cfgDirectiveParseRaw :: Bool
  , cfgDirectiveRun :: TempletteDirective -> m (Either Text [TempletteOutput])
  }

defaultConfig :: (Monad m) => TempletteConfig m
defaultConfig =
  TempletteConfig
    { cfgDelimStart = '{'
    , cfgDelimEnd = '}'
    , cfgDirectiveStart = '$'
    , cfgDirectives = defaultDirectives
    , cfgPreprocess = pure . Right
    , cfgPostprocess = pure . Right
    , cfgImplicitImports = ["import qualified Templette.Prelude"]
    }

defaultDirectives :: (Monad m) => Map Text (DirectiveInfo m)
defaultDirectives =
  Map.fromList
    [
      ( "setup"
      , DirectiveInfo
          { cfgDirectiveParseRaw = True
          , cfgDirectiveRun = Directives.setupDirective
          }
      )
    ,
      ( "define"
      , DirectiveInfo
          { cfgDirectiveParseRaw = False
          , cfgDirectiveRun = Directives.defineDirective
          }
      )
    ,
      ( "call"
      , DirectiveInfo
          { cfgDirectiveParseRaw = False
          , cfgDirectiveRun = Directives.callDirective
          }
      )
    ]
