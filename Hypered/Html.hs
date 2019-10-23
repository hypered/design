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
-- | Generate both the normal and pretty-printed HTML versions.
generate :: FilePath -> Text -> (FilePath -> Html) -> IO ()
generate path title body = do
  generateHtml defaultConfig "generated/min" path title (body path)
  prettyHtml defaultConfig "generated/pretty" path title (body path)
  partialHtml defaultConfig "generated/partial" path title (body path)


------------------------------------------------------------------------------
generateHtml :: Config -> FilePath -> FilePath -> Text -> Html -> IO ()
generateHtml config base path title body = do
  createDirectoryIfMissing True (takeDirectory (base </> path))
  withFile (base </> path) WriteMode $ \h ->
    T.hPutStr h . renderHtml $ document config path title body

prettyHtml :: Config -> FilePath -> FilePath -> Text -> Html -> IO ()
prettyHtml config base path title body = do
  createDirectoryIfMissing True (takeDirectory (base </> path))
  withFile (base </> path) WriteMode $ \h ->
    hPutStr h . Pretty.renderHtml $ document config path title body

-- | Same as prettyHtml but doesn't wrap the content to create a full
-- standalone HTML document.
partialHtml :: Config -> FilePath -> FilePath -> Text -> Html -> IO ()
partialHtml config base path title body = do
  createDirectoryIfMissing True (takeDirectory (base </> path))
  withFile (base </> path) WriteMode $ \h ->
    hPutStr h . Pretty.renderHtml $ body


------------------------------------------------------------------------------
data Font =
    IbmPlex
  | Inter

fontClass :: Font -> String
fontClass IbmPlex = "ibm-plex-sans"
fontClass Inter = "inter"

fontCss :: Font -> String
fontCss IbmPlex = "css/ibm-plex.css"
fontCss Inter = "css/inter.css"

data Config = Config
  { cStaticPath :: FilePath
  , cFont :: Font
  }

defaultConfig = Config
  { cStaticPath = "static"
  , cFont = Inter
  }

------------------------------------------------------------------------------
mkRelativize path = relativize
  where
    depth = length (splitPath path) - 1
    relativize = (joinPath (replicate depth "..") </>)

------------------------------------------------------------------------------
document :: Config -> FilePath -> Text -> Html -> Html
document Config{..} path title body = do
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
          [ cStaticPath </> fontCss cFont
          , cStaticPath </> "css/tachyons.min.v4.11.1.css"
          , cStaticPath </> "css/style.css"
          ]

    H.body ! A.class_ (H.toValue (fontClass cFont)) $
      H.div ! A.class_ "mw9 center ph4 lh-copy" $
        body

-- | Horizontal navigation at the top of a page.
navigation :: FilePath -> Html
navigation path = do
  let depth = length (splitPath path) - 1
      relativize = (joinPath (replicate depth "..") </>)
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

-- | The main content wrapper, at the same level as navigation.
wrap :: Html -> Html
wrap content = do
  H.main ! A.class_ "mw7" $
    H.div ! A.class_ "flex flex-wrap nl3 nr3" $
      content

-- | The footer, at the same level as both navigation and wrap.
footer =
  H.footer ! A.class_ "pv4" $
    H.p ! A.class_ "inline-flex bt b--black-50 pt4 lh-copy" $
      "© Võ Minh Thu, 2017-2019."


-- | The main content, as a left column.
section :: Html -> Html
section content = do
  H.section ! A.class_ "w-100 w-two-thirds-m w-two-thirds-l ph3" $
    content

-- | A right column, with a title and a list of links.
aside :: Html
aside = do
  H.aside ! A.class_ "w-100 w-third-m w-third-l ph3 mt0 mt5-m mt5-l" $ do
    H.h3 ! A.class_ "f5 lh-title mv2" $ "Latest Runs"
    H.div ! A.class_ "nl3 nr3" $
      H.ul ! A.class_ "bg-near-white list pa3" $ do
        H.li ! A.class_ "pv1 bb b--black-10" $
          H.a
            ! A.class_ "black hover-blue"
            ! A.href "../run/264/provisioning.html" $
            "&rarr; #264"
        H.li ! A.class_ "pv1" $
          H.a
            ! A.class_ "black hover-blue"
            ! A.href "../run/263/provisioning.html" $
            "&rarr; #263"

-- | Green variant of a banner.
bannerGreen = banner "bg-green"

-- | Yellow variant of a banner.
bannerYellow = banner "bg-yellow"

-- | Red variant of a banner.
bannerRed = banner "bg-red"

banner bg = H.div ! A.class_ (H.toValue (bg ++ " pa3 white tc fw6 mv3"))

buttonPrimary = H.button
  ! A.class_ "button-reset ph4 pv3 bg-black white ba bw1 b--black"

buttonPrimaryDisabled = H.button
  ! A.class_ "button-reset ph4 pv3 bg-black white ba bw1 b--black o-50"
  ! A.disabled ""

buttonSecondary = H.button
  ! A.class_ "button-reset ph4 pv3 bg-white black ba bw1 b--black"

buttonSecondaryDisabled = H.button
  ! A.class_ "button-reset ph4 pv3 bg-white black ba bw1 b--black o-50"
  ! A.disabled ""

buttonFullWidth = H.button
  ! A.class_ "button-reset ph4 pv3 bg-black white ba bw1 b--black w-100"

-- TODO When pretty-printing the HTML, the first line within the code element
-- is indented, which is not correct.
codeBlock = H.pre ! A.class_ "pre overflow-auto" $ H.code $
  "// this is a comment\n\
  \// this is another comment\n\
  \// this is a slightly longer comment\n"

title = H.title "Hypered"
