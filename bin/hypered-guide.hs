-- | This program generates static HTML pages matching the content of the
-- manually-created guide.html.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)

import Hypered.Html (generateHtml, prettyHtml, Font(..))


------------------------------------------------------------------------------
main :: IO ()
main = do
  generate "index.html" "Hypered style guide"


------------------------------------------------------------------------------
-- | Generate both the normal and pretty-printed HTML versions.
generate :: FilePath -> Text -> IO ()
generate path title = do
  generateHtml Inter "generated/min" path title
  prettyHtml Inter "generated/pretty" path title
