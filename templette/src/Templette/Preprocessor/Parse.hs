{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Templette.Preprocessor.Parse (
  parseTemplette,
  TempletteParseOptions (..),

  -- * Parsed input
  TempletteInput (..),
  TempletteDirective (..),
  TempletteDirectiveContent (..),
  TempletteInputContent (..),
) where

import Control.Monad.Reader (Reader, ask, asks, local, runReader)
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Internal as Internal
import Debug.Trace

data TempletteInput
  = TempletteInputContent TempletteInputContent
  | TempletteInputDirective TempletteDirective
  deriving (Show, Eq)

data TempletteDirective = TempletteDirective
  { directiveName :: Text
  , directiveArgs :: [Text]
  , directiveContent :: TempletteDirectiveContent
  }
  deriving (Show, Eq)

data TempletteDirectiveContent
  = DirectiveContentRaw Text
  | DirectiveContentNodes [TempletteInputContent]
  deriving (Show, Eq)

data TempletteInputContent
  = TempletteInputText Text
  | TempletteInputInterpolate Text
  deriving (Show, Eq)

data TempletteParseOptions = TempletteParseOptions
  { delimStart :: Char
  , delimEnd :: Char
  , directiveStart :: Char
  , rawDirectives :: Set Text
  }

parseTemplette :: TempletteParseOptions -> FilePath -> Text -> Either Text [TempletteInput]
parseTemplette opts fp input =
  first (Text.pack . errorBundlePretty)
    . (`runReader` opts)
    $ runParserT (concat <$> many parseTempletteInput <* eof) fp input

type Parser = ParsecT Void Text (Reader TempletteParseOptions)

parseTempletteInput :: Parser [TempletteInput]
parseTempletteInput =
  choice . map try $
    [ do
        TempletteParseOptions{..} <- ask
        endDirective <- asks (directiveNamed "end")
        _ <- single delimStart *> single directiveStart *> chunk "delims" *> space
        newStart <- anySingle
        newEnd <- anySingle
        _ <- space *> single delimEnd
        traceShowM ("### ASDF", newStart, newEnd)
        local (\opts -> opts{delimStart = newStart, delimEnd = newEnd}) $
          concat <$> manyTill parseTempletteInput (chunk endDirective)
    , (:[]) <$> parseTempletteInputSingle
    ]

parseTempletteInputSingle :: Parser TempletteInput
parseTempletteInputSingle =
  choice . map try $
    [ TempletteInputDirective <$> parseTempletteDirective
    , TempletteInputContent <$> parseTempletteInputContent
    ]

-- TODO: use multiline string logic to strip leading/trailing newline + indentation
parseTempletteDirective :: Parser TempletteDirective
parseTempletteDirective = do
  TempletteParseOptions{..} <- ask

  -- {$directive}
  _ <- single delimStart *> single directiveStart
  directiveName <- takeWhile1P (Just "directive name") (\c -> not (isSpace c) && c /= delimEnd)
  directiveArgs <- Text.words <$> takeWhileP (Just "directive args") (/= delimEnd)
  _ <- single delimEnd
  traceShowM ("directive", directiveName, directiveArgs)

  -- contents + {$end}
  endDirective <- asks (directiveNamed "end")
  directiveContent <-
    if directiveName `Set.member` rawDirectives
      then
        fmap DirectiveContentRaw $
          breakOn endDirective <* chunk endDirective
      else
        fmap DirectiveContentNodes $
          manyTill parseTempletteInputContent (chunk endDirective)

  pure TempletteDirective{..}

parseTempletteInputContent :: Parser TempletteInputContent
parseTempletteInputContent = do
  TempletteParseOptions{..} <- ask
  choice
    [ fmap (TempletteInputText . Text.concat) $
        some . choice $
          [ takeWhile1P (Just "raw text") (`notElem` [delimStart, '\\'])
          , chunk (Text.pack ['\\', delimStart]) *> pure (Text.singleton delimStart)
          , chunk "\\\\" *> pure "\\"
          , chunk "\\"
          ]
    , fmap TempletteInputInterpolate $
        between (single delimStart) (single delimEnd) $
          takeWhile1P (Just "interpolated expr") (/= delimEnd)
    ]

directiveNamed :: Text -> TempletteParseOptions -> Text
directiveNamed name TempletteParseOptions{..} =
  Text.pack [delimStart, directiveStart] <> name <> Text.pack [delimEnd]

{----- Megaparsec utilities -----}

-- https://github.com/mrkkrp/megaparsec/issues/366
breakOn :: Text -> Parser Text
breakOn end = mkParsec $ \s ->
  let (pre, post) = Text.breakOn end (stateInput s)
   in Internal.Reply
        s{stateInput = post, stateOffset = stateOffset s + Text.length pre}
        (if Text.null pre then Internal.NotConsumed else Internal.Consumed)
        (Internal.OK mempty pre)
