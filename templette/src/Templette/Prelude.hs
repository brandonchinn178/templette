{-# LANGUAGE FlexibleInstances #-}

{-|
This is the module that will be automatically imported in the Templette output.
-}
module Templette.Prelude (
  Text,
  text,
  interpolate,
) where

import Data.Text (Text)
import qualified Data.Text as Text

text :: String -> Text
text = Text.pack

-- TODO: move to another file?
class Interpolatable a where
  interpolate :: a -> Text

-- TODO: generalize? use similar thing as 'string-syntax'?
instance Interpolatable Text where
  interpolate = id
instance Interpolatable String where
  interpolate = Text.pack
instance Interpolatable Int where
  interpolate = Text.pack . show
