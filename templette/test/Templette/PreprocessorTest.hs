{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}

module Templette.PreprocessorTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Templette.Config (defaultConfig)
import Templette.Preprocessor

testPreprocess :: Text -> Either Text Text
testPreprocess = runIdentity . preprocessWith defaultConfig "<test>"

testGolden :: String -> FilePath -> [Text] -> TestTree
testGolden name goldenFile inputLines = goldenVsStringDiff name doDiff ("test/goldens/" <> goldenFile) test
  where
    doDiff ref new = ["diff", "-u", ref, new]

    input = Text.unlines inputLines
    test =
      pure . ByteString.Lazy.fromStrict . Text.encodeUtf8 . Text.unlines $
        [ input
        , "====>"
        , ""
        , case testPreprocess input of
            Right s -> s
            Left e -> "ERROR: " <> e
        ]

test =
  testGolden "preprocesses template with interpolation" "preprocess-interpolate.golden" $
    [ "Normal line"
    , "Expression: {repeat 10 '.'}"
    , "Greeting: {greeting}!"
    , "Ending"
    ]

test =
  testGroup "preprocesses template with {$setup} directive" $
    [ testGolden label (Text.unpack goldenFile) $
      [ "# About me"
      , ""
      , "{$setup}"
      , moduleLine
      , importLine
      , "adjective = \"happy\""
      , "{$end}"
      , ""
      , "I am a {adjective} person."
      ]
    | (moduleLabel, moduleKey, moduleLine) <-
        [ ("with module declaration", "module", "module AboutMe where")
        , ("without module declaration", "no-module", "")
        ]
    , (importLabel, importKey, importLine) <-
        [ ("with import", "import", "import Prelude")
        , ("without import", "no-import", "")
        ]
    , let label = unwords [moduleLabel, importLabel]
    , let goldenFile = Text.intercalate "-" ["preprocess-setup", moduleKey, importKey] <> ".golden"
    ]

test =
  testGolden "preprocesses template with multiple {$setup} directives" "preprocess-setup-multiple.golden" $
    [ "# About me"
    , ""
    , "{$setup}"
    , "adjective = \"happy\""
    , "{$end}"
    , ""
    , "I am a {adjective} person."
    , ""
    , "{$setup}"
    , "company = \"Software Inc.\""
    , "{$end}"
    , ""
    , "I work at {company}."
    ]

test =
  testGolden "preprocesses template with {$define} directive" "preprocess-define.golden" $
    [ "{$define description}"
    , "I have {1 + 1} hobbies."
    , "{$end}"
    , ""
    , "Description 1: {description}"
    , "Description 2: {Text.toUpper description}"
    ]

test =
  testGolden "preprocesses template with {$call} directive" "preprocess-call.golden" $
    [ "{$setup}"
    , "import Data.Text (toUpper)"
    , "{$end}"
    , ""
    , "{$call toUpper}"
    , "This will be shouted."
    , "{$end}"
    ]

test =
  testCase "fails on unknown directive" $
    testPreprocess "{$wacky}contents{$end}" @?= Left "Unknown directive: wacky"
