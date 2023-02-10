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

import Control.Monad.Reader (Reader, ask, asks, runReader)
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec

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
    $ runParserT (many parseTempletteInput <* eof) fp input

type Parser = ParsecT Void Text (Reader TempletteParseOptions)

parseTempletteInput :: Parser TempletteInput
parseTempletteInput =
  choice
    [ TempletteInputDirective <$> try parseTempletteDirective
    , TempletteInputContent <$> parseTempletteInputContent
    ]

-- TODO: change delims temporarily with {$delims}
-- TODO: use multiline string logic to strip leading/trailing newline + indentation
parseTempletteDirective :: Parser TempletteDirective
parseTempletteDirective = do
  TempletteParseOptions{..} <- ask
  _ <- single delimStart *> single directiveStart
  directiveName <- takeWhile1P (Just "directive name") (\c -> not (isSpace c) && c /= delimEnd)
  directiveArgs <- Text.words <$> takeWhileP (Just "directive args") (/= delimEnd)
  _ <- single delimEnd
  directiveContent <-
    if directiveName `Set.member` rawDirectives
      then DirectiveContentRaw <$> parseDirectiveContentRaw
      else DirectiveContentNodes <$> parseDirectiveContentNodes
  pure TempletteDirective{..}

parseDirectiveContentRaw :: Parser Text
parseDirectiveContentRaw = do
  end <- asks directiveEnd
  -- https://github.com/mrkkrp/megaparsec/issues/366
  Text.pack <$> manyTill anySingle (chunk end)

parseDirectiveContentNodes :: Parser [TempletteInputContent]
parseDirectiveContentNodes = do
  end <- asks directiveEnd
  manyTill parseTempletteInputContent (chunk end)

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

directiveEnd :: TempletteParseOptions -> Text
directiveEnd TempletteParseOptions{..} =
  Text.pack [delimStart, directiveStart] <> "end" <> Text.pack [delimEnd]
