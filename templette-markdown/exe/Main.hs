import Templette
import Templette.Markdown

main :: IO ()
main = defaultMainWith (registerMarkdownConfig defaultConfig) id
