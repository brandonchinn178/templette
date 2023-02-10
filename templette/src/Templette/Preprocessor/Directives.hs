{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Templette.Preprocessor.Directives (
  setupDirective,
  defineDirective,
  callDirective,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)

import Templette.Preprocessor.Parse (
  TempletteDirective (..),
  TempletteDirectiveContent (..),
  TempletteInputContent,
 )
import Templette.Preprocessor.Transform (TempletteOutput (..), renderInputContent)

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

-- TODO: break out into separate module?
{----- Utilities -----}

expectNoArgs :: Text -> TempletteDirective -> Either Text ()
expectNoArgs name TempletteDirective{..} =
  case directiveArgs of
    [] -> Right ()
    _ -> Left $ "Directive '" <> name <> "' does not take any arguments"

expectOneArg :: Text -> TempletteDirective -> Either Text Text
expectOneArg name TempletteDirective{..} =
  case directiveArgs of
    [arg] -> Right arg
    _ -> Left $ "Directive '" <> name <> "' takes exactly one argument"

expectRawContent :: (HasCallStack) => String -> TempletteDirective -> Text
expectRawContent name TempletteDirective{..} =
  case directiveContent of
    DirectiveContentRaw s -> s
    _ -> unexpectedContent name directiveContent

expectContentNodes :: (HasCallStack) => String -> TempletteDirective -> [TempletteInputContent]
expectContentNodes name TempletteDirective{..} =
  case directiveContent of
    DirectiveContentNodes nodes -> nodes
    _ -> unexpectedContent name directiveContent

unexpectedContent :: (HasCallStack) => String -> TempletteDirectiveContent -> a
unexpectedContent name content =
  error $ "Directive '" <> name <> "' parsed unexpected content: " ++ show content
