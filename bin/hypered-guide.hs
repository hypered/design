-- | This program generates static HTML pages matching the content of the
-- manually-created guide.html.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Hypered.Html
  ( codeBlock, bannerGreen, bannerRed, bannerYellow
  , buttonFullWidth, buttonPrimary, buttonPrimaryDisabled, buttonSecondary
  , buttonSecondaryDisabled
  , generate, navigation)


------------------------------------------------------------------------------
main :: IO ()
main = do

  generate "index.html" "Hypered style guide" $ \_ -> do
    H.ul $ do
      H.li $ H.a ! A.href "navigation.html" $ "Navigation"

      H.li $ H.a ! A.href "banner--green.html" $ "Banner, green"
      H.li $ H.a ! A.href "banner--yellow.html" $ "Banner, yellow"
      H.li $ H.a ! A.href "banner--red.html" $ "Banner, red"

      H.li $ H.a ! A.href "button--primary.html" $ "Button, primary"
      H.li $ H.a ! A.href "button--primary-disabled.html" $ "Button, primary disabled"
      H.li $ H.a ! A.href "button--secondary.html" $ "Button, secondary"
      H.li $ H.a ! A.href "button--secondary-disabled.html" $ "Button, secondary disabled"
      H.li $ H.a ! A.href "button--full-width.html" $ "Button, full width"

      H.li $ H.a ! A.href "code-block.html" $ "Code block"

  -- Horizontal navigation bar:
  -- This is mostly header / nav / a, a, ...

  generate "navigation.html" "Hypered style guide - Navigation" navigation

  -- Banner

  generate "banner--green.html" "Hypered style guide - Banner"
    (const (bannerGreen "Message sent!"))
  generate "banner--yellow.html" "Hypered style guide - Banner"
    (const (bannerYellow "Something might be wrong."))
  generate "banner--red.html" "Hypered style guide - Banner"
    (const (bannerRed "Error, something is wrong."))

  -- Button

  generate "button--primary.html" "Hypered style guide - Button"
    (const (buttonPrimary "Primary Button"))
  generate "button--primary-disabled.html" "Hypered style guide - Button"
    (const (buttonPrimaryDisabled "Primary Button"))
  generate "button--secondary.html" "Hypered style guide - Button"
    (const (buttonSecondary "Secondary Button"))
  generate "button--secondary-disabled.html" "Hypered style guide - Button"
    (const (buttonSecondaryDisabled "Secondary Button"))
  generate "button--full-width.html" "Hypered style guide - Button"
    (const (buttonFullWidth "Primary Button"))

  -- Code block

  generate "code-block.html" "Hypered style guide - Code block"
    (const codeBlock)
