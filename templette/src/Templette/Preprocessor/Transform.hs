{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Templette.Preprocessor.Transform (
  transformWith,
  TempletteTransformOptions (..),

  -- * Transformed output
  TempletteOutput (..),
  renderInputContent,
) where

import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Templette.Preprocessor.Parse (
  TempletteDirective (..),
  TempletteInput (..),
  TempletteInputContent (..),
 )

data TempletteOutput
  = TempletteOutputExpr Text
  | TempletteOutputCode Text
  deriving (Show, Eq)

-- | Hooks for customizing the transformation.
--
-- Functions are all parametrized with a monad @m@, to enable tracking effects during
-- the transformation; e.g. save monadic state when encountering directive A, then
-- do something with that state when encountering directive B.
data TempletteTransformOptions m = TempletteTransformOptions
  { templettePreprocess :: [TempletteInput] -> m (Either Text [TempletteInput])
  , templetteDirectives :: Map Text (TempletteDirective -> m (Either Text [TempletteOutput]))
  , templettePostprocess :: [TempletteOutput] -> m (Either Text [TempletteOutput])
  }

transformWith ::
  (Monad m) =>
  TempletteTransformOptions m
  -> [TempletteInput]
  -> m (Either Text Text)
transformWith options = runExceptT . transform
  where
    transform = preprocess >=> concatMapM resolve >=> postprocess >=> (pure . renderOutput)

    preprocess = ExceptT . templettePreprocess options
    resolve = ExceptT . resolveInput options
    postprocess = ExceptT . templettePostprocess options

    concatMapM f = fmap concat . mapM f

resolveInput :: (Monad m) => TempletteTransformOptions m -> TempletteInput -> m (Either Text [TempletteOutput])
resolveInput TempletteTransformOptions{templetteDirectives} = \case
  TempletteInputContent content -> pure . Right $ [TempletteOutputExpr $ renderInputContentExpr content]
  TempletteInputDirective directive@TempletteDirective{..} ->
    case Map.lookup directiveName templetteDirectives of
      Nothing -> pure . Left $ "Unknown directive: " <> directiveName
      Just resolveDirective -> resolveDirective directive

renderInputContentExpr :: TempletteInputContent -> Text
renderInputContentExpr = \case
  TempletteInputText s -> Text.pack $ show s
  TempletteInputInterpolate expr -> "Templette.Prelude.interpolate (" <> expr <> ")"

-- TODO: enable hooking into this in TempletteTransformOptions?
renderOutput :: [TempletteOutput] -> Text
renderOutput output = Text.unlines $ withImports imports code ++ renderTempletteOutput outputExprs
  where
    (code, outputExprs) =
      partitionEithers . flip map output $ \case
        TempletteOutputCode s -> Left s
        TempletteOutputExpr s -> Right s

    imports = ["import qualified Templette.Prelude"]
    renderTempletteOutput exprs =
      [ "templetteOutput :: Templette.Prelude.Text"
      , "templetteOutput = " <> renderInputContentExprs exprs
      ]

-- | Inject the given imports into the given Haskell code.
withImports :: [Text] -> [Text] -> [Text]
withImports importLines code
  -- if any import lines exist, add the imports before
  | (pre, post) <- break (("import" ==) . firstLexeme) codeLines
  , not (null post) =
      pre <> importLines <> post
  -- if any module lines exist, add the imports after
  | (pre, post) <- break (("module" ==) . firstLexeme) codeLines
  , (moduleLines, whereLine : postModule) <- break ("where" `Text.isSuffixOf`) post =
      pre <> moduleLines <> (whereLine : importLines) <> postModule
  -- otherwise, add the imports to the beginning
  | otherwise = importLines <> codeLines
  where
    codeLines = concatMap Text.lines code
    firstLexeme s =
      case lex $ Text.unpack s of
        (lexeme, _) : _ -> lexeme
        _ -> ""

-- | Exported helper for defining directives. Not used in this file, but defined here
-- to avoid exporting internal functions.
renderInputContent :: [TempletteInputContent] -> Text
renderInputContent = renderInputContentExprs . map renderInputContentExpr

renderInputContentExprs :: [Text] -> Text
renderInputContentExprs exprs =
  Text.intercalate "\n" . concat $
    [ ["mconcat"]
    , [ "  " <> delim <> " " <> expr
      | (isFirst, expr) <- withFirst exprs
      , let delim = if isFirst then "[" else ","
      ]
    , ["  ]"]
    ]
  where
    withFirst = zip (True : repeat False)
