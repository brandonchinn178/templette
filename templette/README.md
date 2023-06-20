# templette

[![GitHub Actions](https://img.shields.io/github/actions/workflow/status/brandonchinn178/templette/ci.yml?branch=main)](https://github.com/brandonchinn178/templette/actions?query=branch%3Amain)

An extensible templating system enabling the execution and interpolation of Haskell expressions.

## Quickstart

1. `cabal install templette-markdown`
1. `echo 'Hello {"wo" ++ "rld"}' > input.ths-md`
1. `templette-markdown --render input.ths-md`

## Recommended extension

`.ths-<ext>`, with the extension for the content being generated. If generating a plain text file, it can be a plain `.ths` extension.

## Overview

`templette` is a GHC preprocessor that takes a `.ths-*` file and converts it into a Haskell file containing a `templetteOutput` export containing the result of the template.

All content in the file will be passed verbatim to the output. The only thing that's special is anything wrapped in `{braces}`. Braces can contain a directive starting with a `$`, or otherwise contain a Haskell expression that will be interpolated into the output.

## Example

The following example uses the `templette-markdown` library, extending `templette` specifically for use with Markdown. [See it rendered here!](examples/readme-snippet.md)

```
# Example

This is a markdown *post*, saved as `example.ths-md`. Everything will be kept output verbatim, except things wrapped in braces, which will interpolate Haskell expressions, such as {show [1, 2, 3]}.

`$setup` blocks will be extracted out of the content and placed at the top-level of the output Haskell file. When this is rendered, you won't see any Haskell code here:

{$setup}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Text qualified as T

shout :: Text -> Text
shout t = "**" <> T.toUpper t <> "!!" <> "**"
{$end}

Things defined in `$setup` blocks can be used in interpolation, like this: {shout "it works"}.

`$call` blocks will call the function with the text content and interpolate the result:

{$call shout}This will be shouted, {T.unwords ["even", "this"]}{$end}

Blocks of content can be set for arbitrary variables with `$define` (which will not be rendered).

{$define description}
> This is the description
> with interpolation: {show $ 1 + 1}
{$end}

This renders the defined description:
{description}

## templette-markdown features

`templette-markdown` adds more directives useful for Literate Haskell-like functionality.

`$code` blocks are like `$setup` blocks, except it'll also be rendered in the content as a Haskell-syntax code block:

{$code}
wordsToLines :: Text -> Text
wordsToLines = T.unlines . T.words
{$end}

`$eval` blocks are rendered as a Haskell-syntax code black (like `$code`), but it'll also interpolate the result of the expression below:

{$eval}
wordsToLines . T.pack $
  "the quick brown fox "
    ++ "jumped over the lazy dog"
{$end}
```
