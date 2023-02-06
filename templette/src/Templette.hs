module Templette (
  defaultMainWith,

  -- * Configuration
  TempletteConfig (..),
  defaultConfig,

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
    preprocessInput origPath inputPath = preprocessWith config origPath <$> Text.readFile inputPath
    printUsage = do
      progName <- getProgName
      putStrLn . unlines $
        [ printf "Usage: %s --preprocess FILE" progName
        , printf "       %s --render FILE" progName
        , printf "       %s PATH INPUT OUTPUT" progName
        ]

data TempletteConfig = TempletteConfig

defaultConfig :: TempletteConfig
defaultConfig = TempletteConfig

preprocessWith :: TempletteConfig -> FilePath -> Text -> Text
preprocessWith _ _ = id

renderWith :: TempletteConfig -> Text -> IO Text
renderWith _ = pure
