{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Define HTML code for the Hypered design system.
module Hypered.Html where

import Data.Text (Text)
import qualified Data.Text.Lazy.IO as T
import System.FilePath (joinPath, splitPath, takeDirectory, FilePath, (</>))
import System.Directory (createDirectoryIfMissing)
import System.IO (hPutStr, withFile, IOMode(WriteMode))
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)


------------------------------------------------------------------------------
generateHtml :: Font -> FilePath -> FilePath -> Text -> IO ()
generateHtml font base path title = do
  createDirectoryIfMissing True (takeDirectory (base </> path))
  withFile (base </> path) WriteMode $ \h ->
    T.hPutStr h . renderHtml $ document font path title

prettyHtml :: Font -> FilePath -> FilePath -> Text -> IO ()
prettyHtml font base path title = do
  createDirectoryIfMissing True (takeDirectory (base </> path))
  withFile (base </> path) WriteMode $ \h ->
    hPutStr h . Pretty.renderHtml $ document font path title


------------------------------------------------------------------------------
data Font =
    IbmPlex
  | Inter

fontClass :: Font -> String
fontClass IbmPlex = "ibm-plex-sans"
fontClass Inter = "inter"

fontCss :: Font -> String
fontCss IbmPlex = "static/css/ibm-plex.css"
fontCss Inter = "static/css/inter.css"


------------------------------------------------------------------------------
document :: Font -> FilePath -> Text -> Html
document font path title = do
  let depth = length (splitPath path) - 1
      relativize = (joinPath (replicate depth "..") </>)
  H.docType
  H.html $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.title (H.toHtml title)
      H.meta ! A.name "viewport"
             ! A.content "width=device-width, initial-scale=1.0"
      H.style $ do
        mapM_ (\a -> H.toHtml ("@import url(" ++ relativize a ++ ");"))
          [ fontCss font
          , "static/css/tachyons.min.v4.11.1.css"
          , "static/css/style.css"
          ]

    H.body ! A.class_ (H.toValue (fontClass font ++ " lh-copy")) $
      H.div ! A.class_ "mw9 center ph4" $
        H.header ! A.class_ "pv4" $
          H.nav ! A.class_ "flex align-items-center lh-copy" $
            mapM_ (\(a, b) ->
              H.a ! A.class_ "mr3 link black hover-blue"
                  ! A.href (H.toValue (relativize a)) $ b)
              [ (".",                       "Entrypoint")
              , ("projects/waveguide.html", "Waveguide")
              , ("projects/station.html",   "Station")
              , ("nubs/work.html",          "Work")
              , ("nubs/",                   "Nubs")
              , ("decks/",                  "Decks")
              , ("edit/",                   "Edit")
              , ("more.html",               "More")
              , ("README.html",             "About")
              ]
