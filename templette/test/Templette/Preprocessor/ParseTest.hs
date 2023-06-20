{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Templette.Preprocessor.ParseTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Templette.Preprocessor.Parse

testParse :: Text -> Either Text [TempletteInput]
testParse = parseTemplette defaultOptions "<test>"

defaultOptions :: TempletteParseOptions
defaultOptions =
  TempletteParseOptions
    { delimStart = '{'
    , delimEnd = '}'
    , directiveStart = '$'
    , rawDirectives = []
    }

test =
  testCase "parses raw input" $ do
    let text = "Some random text here"
    testParse text @?= Right [TempletteInputContent (TempletteInputText text)]

test =
  testCase "parses interpolated input" $
    testParse "I have {1 + 1} ideas"
      @?= Right
        [ TempletteInputContent (TempletteInputText "I have ")
        , TempletteInputContent (TempletteInputInterpolate "1 + 1")
        , TempletteInputContent (TempletteInputText " ideas")
        ]

test =
  testGroup "parses escaped characters" $
    [ testCase "escapes delimiter" $ do
        let options = defaultOptions{delimStart = '!'}
        parseTemplette options "<test>" "\\!" @?= Right [TempletteInputContent (TempletteInputText "!")]
    , testCase "escapes backslash" $
        testParse "\\\\" @?= Right [TempletteInputContent (TempletteInputText "\\")]
    , testCase "does not escape other characters" $
        testParse "\\n" @?= Right [TempletteInputContent (TempletteInputText "\\n")]
    ]

test =
  testCase "parses directives" $
    testParse "A {$foo}B {repeat 10 'C'} D{$end} E"
      @?= Right
        [ TempletteInputContent (TempletteInputText "A ")
        , TempletteInputDirective
            TempletteDirective
              { directiveName = "foo"
              , directiveArgs = []
              , directiveContent =
                  DirectiveContentNodes
                    [ TempletteInputText "B "
                    , TempletteInputInterpolate "repeat 10 'C'"
                    , TempletteInputText " D"
                    ]
              }
        , TempletteInputContent (TempletteInputText " E")
        ]

test =
  testCase "parses raw directives" $ do
    let options = defaultOptions{rawDirectives = ["foo"]}
    parseTemplette options "<test>" "A {$foo} Person{name=\"Alice\"} {$end} B"
      @?= Right
        [ TempletteInputContent (TempletteInputText "A ")
        , TempletteInputDirective
            TempletteDirective
              { directiveName = "foo"
              , directiveArgs = []
              , directiveContent = DirectiveContentRaw " Person{name=\"Alice\"} "
              }
        , TempletteInputContent (TempletteInputText " B")
        ]

test =
  testCase "parses directives with args" $
    testParse "A {$foo 1 asdf}B {'C'} D{$end} E"
      @?= Right
        [ TempletteInputContent (TempletteInputText "A ")
        , TempletteInputDirective
            TempletteDirective
              { directiveName = "foo"
              , directiveArgs = ["1", "asdf"]
              , directiveContent =
                  DirectiveContentNodes
                    [ TempletteInputText "B "
                    , TempletteInputInterpolate "'C'"
                    , TempletteInputText " D"
                    ]
              }
        , TempletteInputContent (TempletteInputText " E")
        ]
