{-# LANGUAGE LambdaCase #-}

module Templette (
  defaultMainWith,

  -- * Configuration
  module Templette.Config,

  -- * Preprocessing
  preprocessWith,

  -- * Rendering
  renderWith,
) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

import Templette.Config
import Templette.Preprocessor

defaultMainWith :: TempletteConfig -> IO ()
defaultMainWith config = do
  args <- getArgs
  when ("--help" `elem` args) $ printUsage >> exitSuccess

  case args of
    ["--preprocess", input] -> preprocessInput input input >>= Text.putStrLn
    ["--render", input] -> preprocessInput input input >>= renderWith config >>= Text.putStrLn
    [fp, input, output] -> preprocessInput fp input >>= Text.writeFile output
    _ -> printUsage >> exitFailure
  where
    preprocessInput origPath inputPath = do
      input <- Text.readFile inputPath
      case preprocessWith config origPath input of
        Left e -> Text.putStrLn e >> exitFailure
        Right output -> pure output
    printUsage = do
      progName <- getProgName
      putStrLn . unlines $
        [ printf "Usage: %s --preprocess FILE" progName
        , printf "       %s --render FILE" progName
        , printf "       %s PATH INPUT OUTPUT" progName
        ]

renderWith :: TempletteConfig -> Text -> IO Text
renderWith _ = pure
