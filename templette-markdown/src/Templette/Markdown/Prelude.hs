{-# LANGUAGE OverloadedStrings #-}

module Templette.Markdown.Prelude (
  renderEvalOutput,
) where

import Data.Text (Text)
import qualified Data.Text as Text

renderEvalOutput :: Text -> Text
renderEvalOutput result =
  Text.unlines
    [ "> _Output_:"
    , "> ```"
    , Text.intercalate "\n"
        . map (\line -> "> " <> line)
        . Text.lines
        $ result
    , "> ```"
    ]
