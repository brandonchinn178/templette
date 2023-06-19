{-# LANGUAGE OverloadedStrings #-}

module Templette.Directives (
  setupDirective,
  defineDirective,
  callDirective,
) where

import Data.Text (Text)
import qualified Data.Text as Text

import Templette.Directives.Utils
import Templette.Preprocessor.Parse (TempletteDirective (..))

setupDirective :: (Monad m) => TempletteDirective -> m (Either Text [TempletteOutput])
setupDirective directive = pure $ do
  expectNoArgs "setup" directive
  let code = expectRawContent "setup" directive
  Right [TempletteOutputCode code]

defineDirective :: (Monad m) => TempletteDirective -> m (Either Text [TempletteOutput])
defineDirective directive = pure $ do
  name <- expectOneArg "define" directive
  let
    content = expectContentNodes "define" directive
    code =
      [ name <> " :: Templette.Prelude.Text"
      , name <> " = " <> renderInputContent content
      ]
  Right [TempletteOutputCode $ Text.unlines code]

callDirective :: (Monad m) => TempletteDirective -> m (Either Text [TempletteOutput])
callDirective directive = pure $ do
  func <- expectOneArg "call" directive
  let content = expectContentNodes "call" directive
  -- TODO: Templette.Prelude.interpolate?
  Right [TempletteOutputExpr $ func <> " (" <> renderInputContent content <> ")"]
