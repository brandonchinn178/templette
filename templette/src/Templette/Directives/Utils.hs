{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Templette.Directives.Utils (
  -- * Directive arguments
  expectNoArgs,
  expectOneArg,

  -- * Directive content
  expectRawContent,
  expectContentNodes,

  -- * Re-exports
  module Templette.Preprocessor.Transform,
) where

import Data.Text (Text)
import GHC.Stack (HasCallStack)

import Templette.Preprocessor.Parse (
  TempletteDirective (..),
  TempletteDirectiveContent (..),
  TempletteInputContent,
 )
import Templette.Preprocessor.Transform

-- | Validate that the directive was passed no args.
expectNoArgs :: Text -> TempletteDirective -> Either Text ()
expectNoArgs name TempletteDirective{..} =
  case directiveArgs of
    [] -> Right ()
    _ -> Left $ "Directive '" <> name <> "' does not take any arguments"

-- | Validate that the directive was passed exactly one arg and return it.
expectOneArg :: Text -> TempletteDirective -> Either Text Text
expectOneArg name TempletteDirective{..} =
  case directiveArgs of
    [arg] -> Right arg
    _ -> Left $ "Directive '" <> name <> "' takes exactly one argument"

-- | Validate that the directive parsed raw content and return it.
expectRawContent :: (HasCallStack) => String -> TempletteDirective -> Text
expectRawContent name TempletteDirective{..} =
  case directiveContent of
    DirectiveContentRaw s -> s
    _ -> unexpectedContent name directiveContent

-- | Validate that the directive parsed content nodes and return them.
expectContentNodes :: (HasCallStack) => String -> TempletteDirective -> [TempletteInputContent]
expectContentNodes name TempletteDirective{..} =
  case directiveContent of
    DirectiveContentNodes nodes -> nodes
    _ -> unexpectedContent name directiveContent

unexpectedContent :: (HasCallStack) => String -> TempletteDirectiveContent -> a
unexpectedContent name content =
  error $ "Directive '" <> name <> "' parsed unexpected content: " ++ show content
