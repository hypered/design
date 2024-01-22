{-# LANGUAGE OverloadedStrings #-}

-- Example script to run Pandoc from a Haskell program and alter its default
-- HTML rendering.

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Pandoc
import Text.Pandoc.Walk (walk)
import Text.HTML.TagSoup (parseTags, renderTags, Tag(..))

main :: IO ()
main = do
  -- Read Markdown content from a file
  markdownInput <- T.readFile "input.md"

  -- Use Pandoc to convert the Markdown to HTML
  let pandocReaderOptions = def
      pandocWriterOptions = def
      maybeHtmlOutput = runPure $ do
        doc <- readMarkdown pandocReaderOptions markdownInput
        writeHtml5String pandocWriterOptions doc

  -- Handle the conversion result and modify HTML
  case maybeHtmlOutput of
    Left err -> error $ "Conversion failed: " ++ show err
    Right htmlOutput -> do
      let htmlWithClasses = addClassToParagraphs htmlOutput
      T.writeFile "output.html" htmlWithClasses

-- Function to add a class to every paragraph. The Pandoc AST doesn't have Attr
-- on Para elements, so we're using Tagsoup. Otherwise, it would be better to
-- use the `walk` function, operating directly on the AST, than
-- parsing/rendering back.
addClassToParagraphs :: Text -> Text
addClassToParagraphs = renderTags . map addClass . parseTags
  where
    addClass (TagOpen "p" attrs) = TagOpen "p" (("class", "x"):attrs)
    addClass tag = tag
