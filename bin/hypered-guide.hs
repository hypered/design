-- | This program generates static HTML pages matching the content of the
-- Storybook component explorer. The generated HTML files are created in the
-- generated/ directory. They are available as production, pretty-printed, and
-- partial HTML.
--
-- They are also exposed in docs/hs/, i.e. at
-- https://hypered.github.io/design/hs/.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isDigit, isUpper, toLower)
import Data.List (nub)
import qualified Data.Text.Lazy.IO as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import System.Environment (getArgs)

import Hypered.Html
  ( cAddWrapper, cFont, Font(IbmPlex)
  , codeBlock, bannerGreen, bannerRed, bannerYellow
  , buttonFullWidth, buttonPrimary, buttonPrimaryDisabled, buttonSecondary
  , buttonSecondaryDisabled, defaultConfig, document
  , exampleLoginForm, exampleRegisterForm, exampleResetForm
  , exampleSidebar, exampleSidePanel
  , footer, generate, generate', loginForm
  , nav, navigation, navigationNoteed, navigationNoteed')
import Hypered.Stories (stories)


------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    -- The document wrapper. This should match `pages/_app.js`.
    ["wrapper"] ->
      putStr (Pretty.renderHtml
        ( document defaultConfig "wrapper.html" "Hypered Design System"
          ( H.preEscapedToHtml ("<!-- CONTENT MARKER -->" :: String)
        )))

    -- Individual components
    ["nav"] -> T.putStr (renderHtml (nav ""))
    ["footer"] -> T.putStr (renderHtml (footer "© Hypered, 2019-2023."))

    -- Stories form Storybook
    ["form--login"] ->
      T.putStr (renderHtml loginForm)
    ["navigation--navigation"] ->
      T.putStr (renderHtml (navigationNoteed))
    ["navigation--navigation-space-between"] ->
      T.putStr (renderHtml (navigationNoteed'))
    ["layout--default"] ->
      T.putStr (renderHtml (nav ""))

    ["list-categories"] -> mapM_ putStrLn (nub ((map fst (tail stories))))
    ["list-stories"] -> mapM_ putStrLn
      (map dashdash (tail stories))
    ["js-import-stories"] -> mapM_ (putStrLn . jsimport)
      (nub (map fst (tail stories)))
    ["js-stories"] -> mapM_ putStrLn
      (map js (tail stories))

    _ -> error "Unsupported argument."


------------------------------------------------------------------------------
dashdash :: (String, String) -> String
dashdash (a, b) = map toLower (a ++ "--" ++ [head b] ++ concatMap f (tail b))
  where
  f c | isUpper c = ['-', c]
  f c | isDigit c = ['-', c]
      | otherwise = [c]

jsimport :: String -> String
jsimport a =
  "var " ++ a ++ " = require(\"./components/" ++ a ++ "/" ++ a ++ ".stories\");"

js :: (String, String) -> String
js (a, b) = unlines
  [ "case '" ++ dashdash (a, b) ++ "':"
  , "  render(" ++ a ++ "." ++ b ++ "());"
  , "  break;"
  ]
