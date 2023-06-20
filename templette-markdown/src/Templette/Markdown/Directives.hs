{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Templette.Markdown.Directives (
  codeDirective,
  evalDirective,
) where

import Data.Text (Text)
import qualified Data.Text as Text

import Templette.Directives.Utils
import Templette.Preprocessor.Parse (
  TempletteDirective (..),
  TempletteInputContent (..),
 )

codeDirective :: (Monad m) => TempletteDirective -> m (Either Text [TempletteOutput])
codeDirective directive = pure $ do
  expectNoArgs "code" directive
  let code = expectRawContent "code" directive
  Right
    [ TempletteOutputCode code
    , TempletteOutputExpr . renderInputContent $
        [ TempletteInputText $ Text.unlines ["```haskell", code, "```"]
        ]
    ]

evalDirective :: (Monad m) => TempletteDirective -> m (Either Text [TempletteOutput])
evalDirective directive = pure $ do
  expectNoArgs "eval" directive
  let code = expectRawContent "eval" directive
  Right
    [ TempletteOutputExpr . renderInputContent $
        [ TempletteInputText $ Text.unlines ["```haskell", code, "```"]
        , TempletteInputInterpolate $ "Templette.Markdown.Prelude.renderEvalOutput $ " <> code
        ]
    ]
