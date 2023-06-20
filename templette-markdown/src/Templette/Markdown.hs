{-# LANGUAGE OverloadedStrings #-}

module Templette.Markdown (
  registerMarkdownConfig,
  markdownDirectives,

  -- * Re-exports
  module Templette,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Templette
import qualified Templette.Markdown.Directives as Directives

registerMarkdownConfig :: (Monad m) => TempletteConfig m -> TempletteConfig m
registerMarkdownConfig config =
  config
    { cfgDirectives = markdownDirectives <> cfgDirectives config
    , cfgImplicitImports = cfgImplicitImports config <> ["import qualified Templette.Markdown.Prelude"]
    }

markdownDirectives :: (Monad m) => Map Text (DirectiveInfo m)
markdownDirectives =
  Map.fromList
    [
      ( "code"
      , DirectiveInfo
          { cfgDirectiveParseRaw = True
          , cfgDirectiveRun = Directives.codeDirective
          }
      )
    ,
      ( "eval"
      , DirectiveInfo
          { cfgDirectiveParseRaw = True
          , cfgDirectiveRun = Directives.evalDirective
          }
      )
    ]
