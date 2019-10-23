-- | This program generates HTML templates that can be used with Pandoc.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Hypered.Html
  ( footer, navigation, title, partialHtml, prettyHtml
  , Config(..), Font(Inter)
  )


config = Config "/static" Inter

------------------------------------------------------------------------------
main :: IO ()
main = do
  prettyHtml config "generated/templates" "default.html" "$title$"
    ("$body$" >> footer)
