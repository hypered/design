-- | This program generates static HTML pages matching the content of the
-- manually-created guide.html.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hypered.Html
  (bannerGreen, bannerRed, bannerYellow, generate, navigation)


------------------------------------------------------------------------------
main :: IO ()
main = do

  -- Horizontal navigation bar:
  -- This is mostly header / nav / a, a, ...
  generate "index.html" "Hypered style guide" navigation

  -- Banner

  generate "banner--green.html" "Hypered style guide - Banner"
    (const (bannerGreen "Message sent!"))
  generate "banner--yellow.html" "Hypered style guide - Banner"
    (const (bannerYellow "Something might be wrong."))
  generate "banner--red.html" "Hypered style guide - Banner"
    (const (bannerRed "Error, something is wrong."))
