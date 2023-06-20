# Example

This is a markdown *post*, saved as `example.ths-md`. Everything will be kept output verbatim, except things wrapped in braces, which will interpolate Haskell expressions, such as [1,2,3].

`$setup` blocks will be extracted out of the content and placed at the top-level of the output Haskell file. When this is rendered, you won't see any Haskell code here:



Things defined in `$setup` blocks can be used in interpolation, like this: **IT WORKS!!**.

`$call` blocks will call the function with the text content and interpolate the result:

**THIS WILL BE SHOUTED, EVEN THIS!!**

Blocks of content can be set for arbitrary variables with `$define` (which will not be rendered).



This renders the defined description:

> This is the description
> with interpolation: 2


## templette-markdown features

`templette-markdown` adds more directives useful for Literate Haskell-like functionality.

`$code` blocks are like `$setup` blocks, except it'll also be rendered in the content as a Haskell-syntax code block:

```haskell

wordsToLines :: Text -> Text
wordsToLines = T.unlines . T.words

```


`$eval` blocks are rendered as a Haskell-syntax code black (like `$code`), but it'll also interpolate the result of the expression below:

```haskell

wordsToLines . T.pack $
  "the quick brown fox "
    ++ "jumped over the lazy dog"

```
> _Output_:
> ```
> the
> quick
> brown
> fox
> jumped
> over
> the
> lazy
> dog
> ```


