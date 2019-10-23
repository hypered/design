-- | This program generates HTML templates that can be used with Pandoc.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Hypered.Html
  ( footer, navigation, title, partialHtml, prettyHtml, wrap
  , Config(..), Font(Inter)
  )


config = Config "/static" Inter

------------------------------------------------------------------------------
main :: IO ()
main = do
  prettyHtml config "generated/templates" "default.html" "$title$"
    (wrap "$body$" >> footer)

  -- TODO Currently reusing the default.html template.
  prettyHtml config "generated/templates" "default-2-cols.html" "$title$"
    (wrap "$body$" >> footer)

  -- TODO Currently reusing the default.html template.
  prettyHtml config "generated/templates" "poster.html" "$title$"
    (wrap "$body$" >> footer)

  -- We probably don't need the footer, navigation, and title partial
  -- templates since they can be generated with the complete templates.

  partialHtml config "generated/templates" "footer.html" "" footer
  partialHtml config "generated/templates" "navigation.html" "" (navigation ".")
  partialHtml config "generated/templates" "title.html" "" title
