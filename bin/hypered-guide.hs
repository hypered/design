-- | This program generates static HTML pages matching the content of the
-- manually-created guide.html.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hypered.Html (generate, navigation)


------------------------------------------------------------------------------
main :: IO ()
main = do
  -- Horizontal navigation bar:
  -- This is mostly header / nav / a, a, ...
  generate "index.html" "Hypered style guide" navigation
