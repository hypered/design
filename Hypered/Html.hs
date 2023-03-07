{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Define HTML code for the Hypered design system.
module Hypered.Html where

import qualified Data.Text.Lazy.IO as T
import           Protolude
import System.FilePath (joinPath, splitPath, takeDirectory, (</>))
import System.Directory (createDirectoryIfMissing)
import Text.Blaze (customAttribute)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)


------------------------------------------------------------------------------
-- | Generate both the normal and pretty-printed HTML versions.
generate :: FilePath -> Text -> (FilePath -> Html) -> IO ()
generate path title body = do
  generate' path title defaultConfig body

-- | Use this version of `generate` and pass it False to render complete
-- exemple pages (i.e. without including the wrapper to present the design
-- system components).
generate' :: FilePath -> Text -> Config -> (FilePath -> Html) -> IO ()
generate' path title conf body = do
  generateHtml conf "generated/min" path title (body path)
  prettyHtml conf "generated/pretty" path title (body path)
  partialHtml conf "generated/partial" path title (body path)


------------------------------------------------------------------------------
generateHtml :: Config -> FilePath -> FilePath -> Text -> Html -> IO ()
generateHtml config base path title body = do
  createDirectoryIfMissing True (takeDirectory (base </> path))
  withFile (base </> path) WriteMode $ \h ->
    T.hPutStr h . renderHtml $ documentFile config path title body

prettyHtml :: Config -> FilePath -> FilePath -> Text -> Html -> IO ()
prettyHtml config base path title body = do
  createDirectoryIfMissing True (takeDirectory (base </> path))
  withFile (base </> path) WriteMode $ \h ->
    hPutStr h . Pretty.renderHtml $ documentFile config path title body'
  where body' = do
          if cAddWrapper config
            then H.div $ do
                   H.a ! A.href "../hs/"
                       $ "back to list"
                   "|"
                   H.code $ H.toHtml path
            else return ()
          body

-- | Same as prettyHtml but doesn't wrap the content to create a full
-- standalone HTML document.
partialHtml :: Config -> FilePath -> FilePath -> Text -> Html -> IO ()
partialHtml _ base path _ body = do
  createDirectoryIfMissing True (takeDirectory (base </> path))
  withFile (base </> path) WriteMode $ \h ->
    hPutStr h . Pretty.renderHtml $ body


------------------------------------------------------------------------------
data Font =
    IbmPlex
  | Inter
  | Font FilePath

fontClass :: Font -> FilePath
fontClass IbmPlex = "hy-ibm-plex"
fontClass Inter = "hy-inter"
fontClass (Font s) = "hy-" ++ s

fontCss :: Font -> FilePath
fontCss IbmPlex = "css/ibm-plex.css"
fontCss Inter = "css/inter.css"
fontCss (Font s) = "css/" ++ s ++ ".css"

data Config = Config
  { cStaticPath :: FilePath
  , cFont :: Font
  , cAddWrapper :: Bool
    -- ^ Choose True when rendering a component, False when rendering a
    -- complete page.
  }

defaultConfig :: Config
defaultConfig = Config
  { cStaticPath = "../static"
  , cFont = IbmPlex
  , cAddWrapper = True
  }


------------------------------------------------------------------------------
mkRelativize :: FilePath -> FilePath -> FilePath
mkRelativize path = relativize
  where
    depth = length (splitPath path) - 1
    relativize = (joinPath (replicate depth "..") </>)


------------------------------------------------------------------------------
-- | This is the main wrapper. This is exposed as
--   $ nix-shell --run "runghc bin/hypered-guide.hs wrapper"
-- and should match the content of `pages/_app.js`.
-- Use this to generate files on disk. Otherwise, see `document` below.
documentFile :: Config -> FilePath -> Text -> Html -> Html
documentFile Config{..} path title body = do
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
        mapM_ (\a -> H.toHtml ("@import url(" <> relativize a <> ");"))
          [ cStaticPath </> fontCss cFont
          , cStaticPath </> "css/tachyons.min.v4.11.1.css"
          , cStaticPath </> "css/style.css"
          , cStaticPath </> "css/styles.css"
          ]

    H.body ! A.class_ (H.toValue (fontClass cFont)) $
      body

-- | Same a `documentFile`, but assume to be served with an available @/static@,
-- and use IBM Plex. I.e. use this from e.g. Servant.
document :: Text -> Html -> Html
document title body = do
  H.docType
  H.html $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.title (H.toHtml title)
      H.meta ! A.name "viewport"
             ! A.content "width=device-width, initial-scale=1.0"
      H.style $ do
        mapM_ (\a -> H.toHtml ("@import url(" <> a <> ");"))
          [ "/static" </> fontCss IbmPlex
          , "/static" </> "css/tachyons.min.v4.11.1.css"
          , "/static" </> "css/style.css"
          , "/static" </> "css/styles.css"
          ]

    H.body ! A.class_ (H.toValue (fontClass IbmPlex)) $
      body

-- | Same a `document`, but accept additional content to insert in the @head@ element.
document' :: Text -> [Html] -> Html -> Html
document' title additional body = do
  H.docType
  H.html $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.title (H.toHtml title)
      H.meta ! A.name "viewport"
             ! A.content "width=device-width, initial-scale=1.0"
      H.style $ do
        mapM_ (\a -> H.toHtml ("@import url(" <> a <> ");"))
          [ "/static" </> fontCss IbmPlex
          , "/static" </> "css/tachyons.min.v4.11.1.css"
          , "/static" </> "css/style.css"
          , "/static" </> "css/styles.css"
          ]
      sequence_ additional

    H.body ! A.class_ (H.toValue (fontClass IbmPlex)) $
      body

-- | This is the div around the main content and footer. It is not part of the
-- `document` function above because within the Storybook examples, it is part
-- of the examples.
wrapper :: Html -> Html
wrapper =
  H.div ! A.class_ "flex flex-column justify-between min-vh-100 mw8 center pa3 pa4-ns lh-copy"


------------------------------------------------------------------------------
anchorBlue :: Text -> Text -> Html
anchorBlue href content =
  H.a ! A.class_ "hy-blue no-underline hy-hover-blue"
      ! A.href (H.toValue href)
      $ H.text content

anchorBlack :: Text -> Text -> Html
anchorBlack href content =
  H.a ! A.class_ "black no-underline hy-hover-blue"
      ! A.href (H.toValue href)
      $ H.text content


------------------------------------------------------------------------------
-- | The main horizontal navigation component. It uses "flex justify-between"
-- so it is better to group its contained links with `div`s. With a single
-- `div`, they are all grouped on the left. With two, the first is grouped to
-- the left and the second to the right.
-- See https://hypered.github.io/design/storybook/?path=/story/navigation--navigation for examples.
nav :: Html -> Html
nav content =
  H.nav ! A.class_ "flex justify-between align-items-center lh-copy mb4 pv3" $
    content

-- | Horizontal navigation at the top of a page.
navigation :: FilePath -> Html
navigation path = do
  let depth = length (splitPath path) - 1
      relativize = (joinPath (replicate depth "..") </>)
  H.header ! A.class_ "pv4" $
    nav $
      H.div $
        mapM_ (\(a, b) ->
          H.a ! A.class_ "link mr3 black hover-blue"
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

-- | Horizontal navigation at the top of a page, at the same level as main
-- wrapper and footer.
navigationNoteed :: Html
navigationNoteed =
  nav $
    H.div $ do
      H.a ! A.class_ "link black hover-blue mr3" ! A.href "#" $ "noteed.com"
      H.a ! A.class_ "link black hover-blue mr3" ! A.href "#" $ "blog"
      H.a ! A.class_ "link black hover-blue" ! A.href "#" $ "not-os"

navigationNoteedX :: Html
navigationNoteedX =
  nav $
    H.div $ do
      H.a ! A.class_ "black hy-hover-blue underline mr3" ! A.href "#" $ "noteed.com"
      H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "blog"
      H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "not-os"

navigationReesd :: Html
navigationReesd =
  nav $
    H.div $ do
      H.a ! A.class_ "link black hover-blue mr3" ! A.href "/" $ "Reesd"

-- | Same as 'navigationNoteed' but with links on the right, except the first
-- one.
navigationNoteed' :: Html
navigationNoteed' =
  nav $ do
    H.div $ do
      H.a ! A.class_ "link black hover-blue mr3" ! A.href "#" $ "noteed.com"
    H.div $ do
      H.a ! A.class_ "link black hover-blue mr3" ! A.href "#" $ "blog"
      H.a ! A.class_ "link black hover-blue" ! A.href "#" $ "not-os"

navigationTemplate :: Html
navigationTemplate =
  H.header $
    nav $
      H.div $ do
        "$for(nav)$"
        H.a ! A.class_ "link mr3 black hover-blue" ! A.href "$nav.href$" $
          "$nav.name$"
        "$endfor$"

-- | Content wrapper, for a blog post, at the same level as navigation and
-- footer.
wrapPost :: Text -> Html -> Html
wrapPost title content =
  H.main $
    H.article ! A.class_ "mw7" $ do
      H.div ! A.class_ "mb4" $ do
        h1 title
        -- TODO
        -- The example /storybook/iframe.html?id=layouts--blog-post has this rule:
        --   H.hr ! A.class_ "mt3 pb3 bt-0 bl-0 br-0 bb b--black"
        -- But it currently conflicts with the custome style.css that makes the
        -- rule short and a bit thick.
        --   H.hr
        -- I'll have to ask Andy how to achieve both in the same document.
      content

-- | The main content wrapper, at the same level as navigation.
wrap :: Html -> Html
wrap content = do
  H.main ! A.class_ "mw7" $
    H.div ! A.class_ "flex flex-wrap nl3 nr3" $
      content

-- | The footer, at the same level as both navigation and wrap.
footer :: Html -> Html
footer content =
  H.footer $ do
    H.hr ! A.class_ "bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4 bw1 b--black"
    H.p ! A.class_ "inline-flex lh-copy" $
      content

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


--------------------------------------------------------------------------------
-- | Green variant of a banner.
bannerGreen :: Html -> Html
bannerGreen = banner "green"

-- | Red variant of a banner.
bannerRed :: Html -> Html
bannerRed = banner "red"

-- | Yellow variant of a banner.
bannerYellow :: Html -> Html
bannerYellow = banner "yellow"

banner :: Text -> Html -> Html
banner color =
  H.div ! A.class_ (H.toValue ("bg-black pa3 white tc fw6 mv3 bl bw3 b--" <> color))
        ! customAttribute "color" (H.toValue color)


--------------------------------------------------------------------------------
blockquote :: Text -> Html
blockquote content =
  H.blockquote ! A.class_ "db bl bw2 pv2 ph3 ml0 mv4 lh-copy" $
    H.span ! A.class_ "i" $
      H.text content

pullQuote' :: Text -> Html
pullQuote' content =
  H.blockquote ! A.class_ "pull-quote relative db pv3 ph4 f4 ml0 mv4 lh-copy" $
    H.span ! A.class_ "i" $
      H.text content

pullQuote :: Text -> Html
pullQuote content =
  H.blockquote ! A.class_ "relative db pv3 ph4 f4 ml0 mv4 lh-copy" $
    H.span ! A.class_ "i" $
      H.text content


--------------------------------------------------------------------------------
buttonPrimary :: Html -> Html
buttonPrimary = H.button
  ! A.class_ "bg-black b--black white hover-light-green ph4 pv3 pointer inline-flex button-reset ba bw1 relative"
  ! customAttribute "variant" "primary"

buttonPrimaryLarge :: Html -> Html
buttonPrimaryLarge = H.button
  ! A.class_ "bg-black b--black white hover-light-green ph3 pb4 pt3 tl w-100 pointer inline-flex button-reset ba bw1 relative"
  ! customAttribute "variant" "primary"

buttonPrimaryDisabled :: Html -> Html
buttonPrimaryDisabled = H.button
  ! A.class_ "bg-black b--black white hover-light-green ph4 pv3 o-50 pointer inline-flex button-reset ba bw1 relative"
  ! customAttribute "variant" "primary"
  ! A.disabled ""

buttonSecondary :: Html -> Html
buttonSecondary = H.button
  ! A.class_ "bg-white hover-bg-light-gray b--black black ph4 pv3 pointer inline-flex button-reset ba bw1 relative"
  ! customAttribute "variant" "secondary"

buttonSecondaryLarge :: Html -> Html
buttonSecondaryLarge = H.button
  ! A.class_ "bg-white hover-bg-light-gray b--black black ph3 pb4 pt3 tl w-100 pointer inline-flex button-reset ba bw1 relative"
  ! customAttribute "variant" "secondary"

buttonSecondaryDisabled :: Html -> Html
buttonSecondaryDisabled = H.button
  ! A.class_ "bg-white hover-bg-light-gray b--black black ph4 pv3 o-50 pointer inline-flex button-reset ba bw1 relative"
  ! customAttribute "variant" "secondary"
  ! A.disabled ""

buttonFullWidth :: Html -> Html
buttonFullWidth = H.button
  ! A.class_ "bg-black b--black white hover-light-green ph4 pv3 w-100 pointer inline-flex button-reset ba bw1 relative"
  ! customAttribute "variant" "primary"


--------------------------------------------------------------------------------
buttonLinkPrimary :: Html -> Html
buttonLinkPrimary = H.a
  ! A.class_ "bg-black b--black white ph4 pv3 tc dib no-underline ba bw1"

buttonLinkPrimaryLarge :: Html -> Html
buttonLinkPrimaryLarge = H.a
  ! A.class_ "bg-black b--black white ph3 pb4 pt3 tl w-100 dib no-underline ba bw1"

buttonLinkPrimaryDisabled :: Html -> Html
buttonLinkPrimaryDisabled = H.a
  ! A.class_ "bg-black b--black white ph4 pv3 tc o-50 dib no-underline ba bw1"

buttonLinkSecondary :: Html -> Html
buttonLinkSecondary = H.a
  ! A.class_ "bg-white b--black black ph4 pv3 tc dib no-underline ba bw1"

buttonLinkSecondaryLarge :: Html -> Html
buttonLinkSecondaryLarge = H.a
  ! A.class_ "bg-white b--black black ph3 pb4 pt3 tl w-100 dib no-underline ba bw1"

buttonLinkSecondaryDisabled :: Html -> Html
buttonLinkSecondaryDisabled = H.a
  ! A.class_ "bg-white b--black black ph4 pv3 tc o-50 dib no-underline ba bw1"

buttonLinkFullWidth :: Html -> Html
buttonLinkFullWidth = H.a
  ! A.class_ "bg-black b--black white ph4 pv3 tc w-100 dib no-underline ba bw1"


--------------------------------------------------------------------------------
checkboxDefault :: Text -> Html
checkboxDefault content =
  H.label ! A.class_ "flex items-center mb2 flex" $ do
    H.input ! A.type_ "checkbox" ! A.class_ "hy-checkbox w1 h1" ! A.checked ""
    H.div ! A.class_ "ml1" $ H.text content

checkboxPill :: Text-> Html
checkboxPill content =
  H.label ! A.class_ "flex items-center mb2 flex" $ do
    H.input ! A.type_ "checkbox" ! A.class_ "hy-checkbox w1 h1 br-pill" ! A.checked ""
    H.div ! A.class_ "ml1" $ H.text content


--------------------------------------------------------------------------------
-- When pretty-printing the HTML with a H.code element, the first line
-- within the code element is indented, which is not correct. Using the
-- non-pretty-printing renderer is advised.
codeblock :: Text -> Html
codeblock content = H.pre ! A.class_ "pre overflow-auto" $
  H.code ! A.class_ "code" $
    H.text content

codeblockEditable :: Text -> Html
codeblockEditable content =
  H.pre ! A.class_ "pre hy-editable pv3 ph3 mv0 overflow-auto"
        ! A.contenteditable "true"
        ! A.spellcheck "false" $
    H.code ! A.class_ "code" $
      H.text content

codeblockTextArea :: Text -> Html
codeblockTextArea content =
  H.pre ! A.class_ "pv3 ph3 mv0 relative pre overflow-auto hy-editable"
        ! A.spellcheck "false" $
    H.textarea ! A.class_ "code db w-100 h5 input-reset bn pa0 ma0 outline-0" $
      H.text content

codeblockEditableBottomButton :: Text -> Html
codeblockEditableBottomButton content = H.form $ do
  H.div ! A.class_ "flex flex-wrap items-center bt bb justify-between pv3 mh3" $
    H.div "show fetchgit:README.md"
  H.pre ! A.class_ "pre hy-editable pv3 ph3 mv0 overflow-auto"
        ! A.contenteditable "true"
        ! A.spellcheck "false" $
    H.code ! A.class_ "code" $
      H.text content
  H.div ! A.class_ "bt pt3 mh3" $
    H.input ! A.type_ "submit"
            ! A.class_ "db button-reset bg-black pa3 white br2 bn"
            ! A.value "Submit"

codeblockEditableToolbarButton :: Text -> Html
codeblockEditableToolbarButton content = H.form $ do
  H.div ! A.class_ "flex flex-wrap items-center bt bb justify-between pv2 mh3" $ do
    H.div "show fetchgit:README.md"
    H.div $
      H.input ! A.type_ "submit"
              ! A.class_ "button-reset bg-black ph3 pv2 white br2 bn"
              ! A.value "Save"
  H.pre ! A.class_ "pre hy-editable pv3 ph3 mv0 overflow-auto"
        ! A.contenteditable "true"
        ! A.spellcheck "false" $
    H.code ! A.class_ "code" $
      H.text content

codeblockTextAreaBottomButton :: Text -> Html
codeblockTextAreaBottomButton content = H.form $ do
  H.div ! A.class_ "flex flex-wrap items-center bt bb justify-between pv3 mh3" $
    H.div "show fetchgit:README.md"
  H.pre ! A.class_ "pv3 ph3 mv0 relative pre overflow-auto hy-editable"
        ! A.spellcheck "false" $
    H.textarea ! A.class_ "code db w-100 h5 input-reset bn pa0 ma0 outline-0" $
      H.text content
  H.div ! A.class_ "bt pt3 mh3" $
    H.input ! A.type_ "submit"
            ! A.class_ "db button-reset bg-black pa3 white br2 bn"
            ! A.value "Submit"

codeblockTextAreaToolbarButton :: Text -> Html
codeblockTextAreaToolbarButton content = H.form $ do
  H.div ! A.class_ "flex flex-wrap items-center bt bb justify-between pv2 mh3" $ do
    H.div "show fetchgit:README.md"
    H.div $
      H.input ! A.type_ "submit"
              ! A.class_ "button-reset bg-black ph3 pv2 white br2 bn"
              ! A.value "Save"
  H.pre ! A.class_ "pv3 ph3 mv0 relative pre overflow-auto hy-editable"
        ! A.spellcheck "false" $
    H.textarea ! A.class_ "code db w-100 h5 input-reset bn pa0 ma0 outline-0" $
      H.text content


--------------------------------------------------------------------------------
colorText :: Html
colorText = do
  H.div ! A.class_ "hy-blue mb3" $ "Hypered"
  H.div ! A.class_ "hy-red mb3" $ "Hypered"
  H.div ! A.class_ "hy-green mb3" $ "Hypered"
  H.div ! A.class_ "hy-yellow mb3" $ "Hypered"

colorBackground :: Html
colorBackground = do
  H.div ! A.class_ "flex items-center mb3" $ do
    H.div ! A.class_ "hy-bg-blue w2 h2 mr2" $ ""
    H.div "Blue"
  H.div ! A.class_ "flex items-center mb3" $ do
    H.div ! A.class_ "hy-bg-red w2 h2 mr2" $ ""
    H.div "Red"
  H.div ! A.class_ "flex items-center mb3" $ do
    H.div ! A.class_ "hy-bg-green w2 h2 mr2" $ ""
    H.div "Green"
  H.div ! A.class_ "flex items-center mb3" $ do
    H.div ! A.class_ "hy-bg-yellow w2 h2 mr2" $ ""
    H.div "Yellow"

colorSamples :: Html
colorSamples =
  H.div ! A.class_ "pa4" $
    H.div ! A.class_ "flex flex-wrap nl2 nr2 mw8" $ do
      H.div ! A.class_ "w-100 w-50-l ph2 mb3" $
        H.div ! A.class_ "hy-bg-blue light-green aspect-ratio aspect-ratio--4x3 relative" $
          card
      H.div ! A.class_ "w-100 w-50-l ph2 mb3" $
        H.div ! A.class_ "hy-bg-red light-yellow aspect-ratio aspect-ratio--4x3 relative" $
          card
      H.div ! A.class_ "w-100 w-50-l ph2 mb3" $
        H.div ! A.class_ "hy-bg-green black aspect-ratio aspect-ratio--4x3 relative" $
          card
      H.div ! A.class_ "w-100 w-50-l ph2 mb3" $
        H.div ! A.class_ "hy-bg-yellow hy-red aspect-ratio aspect-ratio--4x3 relative" $
          card
      H.div ! A.class_ "w-100 w-50-l ph2 mb3" $
        H.div ! A.class_ "hy-bg-blue white aspect-ratio aspect-ratio--4x3 relative" $
          card
      H.div ! A.class_ "w-100 w-50-l ph2 mb3" $
        H.div ! A.class_ "hy-bg-red white aspect-ratio aspect-ratio--4x3 relative" $
          card
      H.div ! A.class_ "w-100 w-50-l ph2 mb3" $
        H.div ! A.class_ "hy-bg-green white aspect-ratio aspect-ratio--4x3 relative" $
          card
      H.div ! A.class_ "w-100 w-50-l ph2 mb3" $
        H.div ! A.class_ "hy-bg-yellow white aspect-ratio aspect-ratio--4x3 relative" $
          card
 where
  card =
    H.div ! A.class_ "aspect-ratio--object flex items-stretch" $ do
      H.div ! A.class_ "flex flex-column justify-between pa4 h-100" $ do
        H.h2 ! A.class_ "f2 f1-m f1-l mv2 fw6 tracked-tight lh-title" $ do
          "Samples "
          H.span ! A.class_ "o-50" $ "↗"
        H.p ! A.class_ "f6 f5-m f5-l lh-copy" $
          "The user interface (UI), in the industrial design field of human-computer interaction, is the space where interactions between humans and machines occur."
      H.div ! A.class_ "bl w4" $
        H.div ! A.class_ "flex justify-between items-center pa3 h-100"
              ! A.style "writing-mode:vertical-lr;text-orientation:mixed" $ do
          H.div ! A.class_ "b" $ "Hypered Design System"
          H.div $ "Volume 001"


--------------------------------------------------------------------------------
containerWithLabelDefault :: Html
containerWithLabelDefault =
  H.div ! A.class_ "pa6 mw7 center" $
    H.div $ do
      H.label ! A.class_ "dib bg-red white f6 mv0 pv1 ph2" $ "Warning!"
      H.div ! A.class_ "ba b--red debug-grid-16" $
        H.div ! A.class_ "aspect-ratio aspect-ratio--3x4 relative" $
          H.div ! A.class_ "aspect-ratio--object pa4" $ do
            H.h2 ! A.class_ "f-headline fw9 tracked-tight lh-title mv3" $ "Error 50x"
            p $
              "Looks like the page that you're looking for is not available. Please click here to return to the home page."


--------------------------------------------------------------------------------
dropdownDefault :: Html
dropdownDefault =
  H.select ! A.class_ "hy-dropdown br1 black ba b--black bw1 br0 pv2 f6" $ do
    H.option ! A.value "" $ "Select from dropdown"
    H.option ! A.value "apple" $ "Apple"
    H.option ! A.value "banana" $ "Banana"
    H.option ! A.value "cherry" $ "Cherry"


--------------------------------------------------------------------------------
footerDefault :: Html
footerDefault = footer ""


------------------------------------------------------------------------------
-- | Login form
-- https://hypered.design/storybook/?path=/story/form--login
loginForm :: Html
loginForm = do
  formPost "/echo/login" $ do
    formBody "Log in to Appname" $ do
      H.div ! A.class_ "mv3" $
        H.div ! A.class_ "mb3" $ do
          H.label ! A.class_ "db fw6 mv1" $ "Username"
                  ! A.for "username"
          H.input ! A.type_ "text"
                  ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv3 ph2 w-100 outline-0 border-box"
                  ! A.name "username"
                  ! A.id "username"
          H.div ! A.class_ "mv1 h1 red fw5" $
            "You have entered an invalid email"
      H.div ! A.class_ "mv3" $
        H.div ! A.class_ "mb3" $ do
          H.label ! A.class_ "db fw6 mv1" $ "Password"
                  ! A.for "password"
          H.input ! A.type_ "password"
                  ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv3 ph2 w-100 outline-0 border-box"
                  ! A.placeholder ""
                  ! A.name "password"
                  ! A.id "password"
          H.div ! A.class_ "mv1 h1 red fw5" $ ""
      formLink "#" "Reset password"
    formButtons $ do
      H.button ! A.class_ "bg-white hover-bg-light-gray b--black black ph3 pb4 pt3 tl w-100 pointer inline-flex button-reset ba bw1 relative"
               ! customAttribute "variant" "secondary"
               $ "Register"
      H.button ! A.class_ "bg-black b--black white hover-light-green ph3 pb4 pt3 tl w-100 pointer inline-flex button-reset ba bw1 relative"
               ! customAttribute "variant" "primary"
               $ "Log in —>"

-- This is a slightly more compact form than above.
loginFormReesd :: Html
loginFormReesd = do
  formPost "/echo/login" $ do
    formBody "Log in to Reesd" $ do
      formFieldTextSmall "username" "Username" Nothing
        -- Just "You have entered an invalid email"
      formFieldPasswordSmall "password" "Password" Nothing
      formLink "/reset" "Reset password"
    formButtons $ do
      formButtonLink "/register" "Register"
      formButton "Log in —>"

formPost :: H.AttributeValue -> Html -> Html
formPost href content =
  H.form ! A.class_ "bg-white mw6"
         ! A.method "POST"
         ! A.action href
         $ content

formBody :: Text -> Html -> Html
formBody title content =
  H.div ! A.class_ "pa4 bt br bl b--black bw1" $ do
    H.h2 $ H.text title
    content

formFieldTextSmall :: H.AttributeValue -> Text -> Maybe Text -> Html
formFieldTextSmall id label merror =
  H.div ! A.class_ "mv3" $
    H.div ! A.class_ "mb3" $ do
      H.label ! A.class_ "db fw6 mv1" $ H.text label
              ! A.for id
      H.input ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv2 ph2 w-100 outline-0 border-box"
              ! A.name id
              ! A.id id
              ! A.type_ "text"
              ! A.placeholder ""
      maybe mempty ((H.div ! A.class_ "mv1 h1 red fw5") . H.text) merror

formFieldPasswordSmall :: H.AttributeValue -> Text -> Maybe Text -> Html
formFieldPasswordSmall id label merror =
  H.div ! A.class_ "mv3" $
    H.div ! A.class_ "mb3" $ do
      H.label ! A.class_ "db fw6 mv1" $ H.text label
              ! A.for id
      H.input ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv2 ph2 w-100 outline-0 border-box"
              ! A.name id
              ! A.id id
              ! A.type_ "password"
              ! A.placeholder ""
      maybe mempty ((H.div ! A.class_ "mv1 h1 red fw5") . H.text) merror

formFieldEmailSmall :: H.AttributeValue -> Text -> Maybe Text -> Html
formFieldEmailSmall id label merror =
  H.div ! A.class_ "mv3" $
    H.div ! A.class_ "mb3" $ do
      H.label ! A.class_ "db fw6 mv1" $ H.text label
              ! A.for id
      H.input ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv2 ph2 w-100 outline-0 border-box"
              ! A.label id
              ! A.name id
              ! A.id id
              ! A.type_ "email"
              ! A.placeholder ""
      maybe mempty ((H.div ! A.class_ "mv1 h1 red fw5") . H.text) merror

formButtons :: Html -> Html
formButtons content =
  H.div ! A.class_ "flex justify-between" $ content

formLink :: H.AttributeValue-> Text -> Html
formLink href content =
  H.a ! A.class_ "black no-underline hy-hover-blue"
      ! A.href href
      $ H.text content

formButtonLink :: H.AttributeValue -> Text -> Html
formButtonLink href content =
  H.a ! A.class_ "bg-white b--black black ph3 pb4 pt3 tl w-100 dib no-underline ba bw1"
      ! A.href href
      $ H.text content

formButton :: Text -> Html
formButton content =
  H.button ! A.class_ "bg-black b--black white ph3 pb4 pt3 tl w-100 button-reset ba bw1" $
    H.text content


--------------------------------------------------------------------------------
hrDefault :: Html
hrDefault =
  H.hr ! A.class_ "bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4 bw1 b--black"

hrDivider :: Html
hrDivider =
  H.hr ! A.class_ "mt3 pb3 bt-0 bl-0 br-0 bb b--black"


--------------------------------------------------------------------------------
imageDefault :: Html
imageDefault =
  H.figure ! A.class_ "mv0" $
    H.img ! A.class_ "img v-top" ! A.src "img/placeholder.svg"

imageNegativePull :: Html
imageNegativePull =
  H.figure ! A.class_ "mv0 nl4 nr4" $
    H.img ! A.class_ "img v-top" ! A.src "img/placeholder.svg"

imageFullWidth :: Html
imageFullWidth =
  H.figure ! A.class_ "mv0 w-100 mh0" $
    H.img ! A.class_ "img v-top" ! A.src "img/placeholder.svg"

imageWithCaption :: Html
imageWithCaption =
  H.figure ! A.class_ "mv0" $ do
    H.img ! A.class_ "img v-top" ! A.src "img/placeholder.svg"
    H.figcaption ! A.class_ "f6 gray mv1 tc" $
      "This is a placeholder image."

imageExamples :: Html
imageExamples =
  wrapper $ do
    H.div $ do
      H.header $
        H.nav ! A.class_ "flex justify-between align-items-center lh-copy mb4 pv3" $
          H.div $ do
            H.a ! A.class_ "black hy-hover-blue underline mr3" ! A.href "#" $
              "noteed.com"
            H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "blog"
            H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "not-os"
      H.main $
        H.article ! A.class_ "mw7 cf" $ do
          h1 "Using the Image component"
          p $ do
            "In this example we will be exploring different ways of using the Image component. There are three variations of the Image component namely; "
            H.i "default"
            ", "
            H.i "negative pull"
            ", and, "
            H.i "full-width"
            ". There is also an optional option should you need to add one."
          h2 "Default"
          H.figure ! A.class_ "mv0" $ do
            H.img ! A.class_ "img v-top" ! A.src "img/placeholder.svg"
          h2 "With Caption"
          H.figure ! A.class_ "mv0" $ do
            H.img ! A.class_ "img v-top" ! A.src "img/placeholder.svg"
            H.figcaption ! A.class_ "f6 gray mv1 tc" $
              "This is an image with a caption."
          h2 "With Negative Pull"
          H.figure ! A.class_ "mv0 nl4 nr4" $ do
            H.img ! A.class_ "img v-top" ! A.src "img/placeholder.svg"
            H.figcaption ! A.class_ "f6 gray mv1 tc" $
              "This is an image with negative margins applied to it."
          h2 "Full Width Image"
          H.figure ! A.class_ "mv0 w-100 mh0" $ do
            H.img ! A.class_ "img v-top" ! A.src "img/placeholder.svg"
            H.figcaption ! A.class_ "f6 gray mv1 tc" $
              "This is an image that takes up the whole width of its parent."
    H.footer $ do
      H.hr ! A.class_ "bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4 bw1 b--black"
      H.p ! A.class_ "inline-flex lh-copy" $
        "© Hypered, 2019-2023."


--------------------------------------------------------------------------------
inputTextExample :: Html
inputTextExample =
  H.div ! A.class_ "mv3" $
    H.div ! A.class_ "mb3" $ do
      H.label ! A.class_ "db fw6 mv1" $ "Full Name"
      H.input ! A.type_ "text"
              ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv3 ph2 w-100 outline-0 border-box"
              ! A.placeholder "John Appleseed"
      H.div ! A.class_ "mv1 h1 red fw5" $ mempty

inputPasswordExample :: Html
inputPasswordExample =
  H.div ! A.class_ "mv3" $
    H.div ! A.class_ "mb3" $ do
      H.label ! A.class_ "db fw6 mv1" $ "Password"
      H.input ! A.type_ "password"
              ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv3 ph2 w-100 outline-0 border-box"
              ! A.value "hello, world"
      H.div ! A.class_ "mv1 h1 red fw5" $ mempty

inputNumberExample :: Html
inputNumberExample =
  H.div ! A.class_ "mv3" $
    H.div ! A.class_ "mb3" $ do
      H.label ! A.class_ "db fw6 mv1" $ "Quantity"
      H.input ! A.type_ "number"
              ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv3 ph2 w-100 outline-0 border-box"
              ! A.value "20"
      H.div ! A.class_ "mv1 h1 red fw5" $ mempty

inputWithMessageExample :: Html
inputWithMessageExample =
  H.div ! A.class_ "mv3" $
    H.div ! A.class_ "mb3" $ do
      H.label ! A.class_ "db fw6 mv1" $ "Email Address"
      H.input ! A.type_ "email"
              ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv3 ph2 w-100 outline-0 border-box"
              ! A.value "alice@example.com"
      H.div ! A.class_ "mv1 h1 red fw5" $ "Not a valid email."

inputUsage :: Html
inputUsage =
  H.div ! A.class_ "mw6 pa4" $ do
    H.div ! A.class_ "mv3" $
      H.div ! A.class_ "mb3" $ do
        H.label ! A.class_ "db fw6 mv1" $ "Full Name"
        H.input ! A.type_ "text"
                ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv3 ph2 w-100 outline-0 border-box"
                ! A.placeholder "John Appleseed"
        H.div ! A.class_ "mv1 h1 red fw5" $ mempty
    H.div ! A.class_ "mv3" $
      H.div ! A.class_ "mb3" $ do
        H.label ! A.class_ "db fw6 mv1" $ "Email Address"
        H.input ! A.type_ "email"
                ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv3 ph2 w-100 outline-0 border-box"
                ! A.value "alice@example.com"
        H.div ! A.class_ "mv1 h1 red fw5" $ "Not a valid email."
    H.div ! A.class_ "mv3" $
      H.div ! A.class_ "mb3" $ do
        H.label ! A.class_ "db fw6 mv1" $ "Password"
        H.input ! A.type_ "password"
                ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv3 ph2 w-100 outline-0 border-box"
                ! A.value "hello, world"
        H.div ! A.class_ "mv1 h1 red fw5" $ mempty


--------------------------------------------------------------------------------
layoutDefault :: Html
layoutDefault =
  wrapper $ do
    H.div $ do
      H.header $
        H.nav ! A.class_ "flex justify-between align-items-center lh-copy mb4 pv3" $
          H.div $ do
            H.a ! A.class_ "black hy-hover-blue underline mr3" ! A.href "#" $
              "noteed.com"
            H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "blog"
            H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "not-os"
      "Content goes here"
    H.footer $ do
      H.hr ! A.class_ "bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4 bw1 b--black"
      H.p ! A.class_ "inline-flex lh-copy" $ "© Hypered, 2019-2023."

layoutBlogList :: Html
layoutBlogList =
  wrapper $ do
    H.div $ do
      H.header $
        H.nav ! A.class_ "flex justify-between align-items-center lh-copy mb4 pv3" $
          H.div $ do
            H.a ! A.class_ "black hy-hover-blue underline mr3" ! A.href "#" $
              "noteed.com"
            H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "blog"
            H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "not-os"
      H.main $ do
        h1 "The Hypered Publication"
        H.ul ! A.class_ "list pl0 mw6 mt4" $ do
          H.li ! A.class_ "pv3" $ do
            h3 "Starting with NixOps (and thus Nix and NixOS)"
            H.time ! A.datetime "2018-12-08" $ "2019-08-21"
            p $ "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
            H.a ! A.href "#" ! A.class_ "black b no-underline" $ "Continue reading..."
          H.li ! A.class_ "pv3" $ do
            h3 "Exposing a local server through HAProxy using a reverse SSH tunnel"
            H.time ! A.datetime "2018-12-08" $ "2019-08-21"
            p $ "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
            H.a ! A.href "#" ! A.class_ "black b no-underline" $ "Continue reading..."
    H.footer $ do
      H.hr ! A.class_ "bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4 bw1 b--black"
      H.p ! A.class_ "inline-flex lh-copy" $ "© Hypered, 2019-2023."

layoutBlogPost1 :: Html
layoutBlogPost1 =
  wrapper $ do
    H.div $ do
      H.header $
        H.nav ! A.class_ "flex justify-between align-items-center lh-copy mb4 pv3" $
          H.div $ do
            H.a ! A.class_ "black hy-hover-blue underline mr3" ! A.href "#" $
              "noteed.com"
            H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "blog"
            H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "not-os"
      H.main $ do
        H.article ! A.class_ "mw7 cf" $ do
          H.div ! A.class_ "mb4" $ do
            h1 "Static binaries"
          p $
            "If you consider adopting a programming language, consider one where you can create statically-linked executables. This makes things so much easier."
          H.hr ! A.class_ "bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4 bw1 b--black"
          h2 "E.g., compared to Docker"
          p $
            "With a statically-linked binary, provisioning a program on a remote host is just uploading it. With Docker, you need to install and maintain docker-engine. Ideally you should have a nice way to provision the remote host with Docker, but then, why not use that nice way to provision other stuff directly ?"
          p $
            "Docker is much more than packaging, but it seems a lot of people use it for that purpose."
          p $
            "By the way, statically-linked executables make building Docker images a breeze."
          p $
            "If you use Docker to deploy, you probably want a private registry, which is another piece to maintain. Static binaries are trivial to host on a static site. Or to mirror. And so on. Simplicity goes a long way."
          H.hr ! A.class_ "bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4 bw1 b--black"
          p $ do
            "See also: "
            H.a ! A.class_ "no-underline hy-hover-blue" ! A.href "#" $ "In praise of simplicity"
          H.hr ! A.class_ "bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4 bw1 b--black"
          p $ do
            "Related: "
            H.a ! A.class_ "no-underline hy-hover-blue" ! A.href "#" $ "Learn packaging"
    H.footer $ do
      H.hr ! A.class_ "bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4 bw1 b--black"
      H.p ! A.class_ "inline-flex lh-copy" $ "© Võ Minh Thu, 2017-2023."

layoutBlogPost2 :: Html
layoutBlogPost2 =
  wrapper $ do
    H.div $ do
      H.header $
        H.nav ! A.class_ "flex justify-between align-items-center lh-copy mb4 pv3" $
          H.div $ do
            H.a ! A.class_ "black hy-hover-blue underline mr3" ! A.href "#" $
              "noteed.com"
            H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "blog"
            H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "not-os"
      H.main $ do
        H.article ! A.class_ "mw7 cf" $ do
          H.div ! A.class_ "mb4" $ do
            h1 "Starting with NixOps (and thus Nix and NixOS)"
            H.hr ! A.class_ "mt3 pb3 bt-0 bl-0 br-0 bb b--black"
          h2 "Introduction"
          p $ "Given the three above derivations, it is possible to generate the appropriate qemu-kvm invocation as a script, runvm. runvm is the main entry point to start playing and understanding not-os. Follow the link, and enjoy!"
          H.ul ! A.class_ "hy-ff-tab-num mv3" $ do
            H.li ! A.class_ "mv1" $ "a kernel (config.system.build.kernel)"
            H.li ! A.class_ "mv1" $ "an initrd (config.system.build.initialRamdisk)"
            H.li ! A.class_ "mv1" $ "a rootfs (config.system.build.squashfs)"
          p $ "This is a project of Michael Bishop (cleverca22 on GitHub, clever on IRC). I modified it just a bit to make it possible to generate this documentation."
          h3 "Sub-points"
          H.ol ! A.class_ "hy-ff-tab-num mv3" $ do
            H.li ! A.class_ "mv1" $ "Item one"
            H.li ! A.class_ "mv1" $ "Item two"
            H.li ! A.class_ "mv1" $ "Item three"
          h4 "Some quotes"
          H.pre ! A.class_ "pre overflow-auto" $
            H.code ! A.class_ "code" $
             "┌──────────────────────────────────┬─────────┬────────────────┐\n\
             \│               Col1               │  Col2   │ Numeric Column │\n\
             \├──────────────────────────────────┼─────────┼────────────────┤\n\
             \│ Value 1                          │ Value 2 │           10.0 │\n\
             \│ Separate                         │ cols    │       -2,027.1 │\n\
             \│ This is a row with only one cell │         │                │\n\
             \└──────────────────────────────────┴─────────┴────────────────┘\n"
          H.blockquote ! A.class_ "db bl bw2 pv2 ph3 ml0 mv4 lh-copy" $
            H.span ! A.class_ "i" $
              "To follow along, you can clone the Git repository and run each nix-build command as they appear at the top of each page."
          h3 "Ending points"
          H.blockquote ! A.class_ "pull-quote relative db pv3 ph4 f4 ml0 mv4 lh-copy" $
            H.span ! A.class_ "i" $
              "To follow along, you can clone the Git repository and run each nix-build command as they appear at the top of each page."
          p $
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
        H.aside ! A.class_ "mt4" $
          H.nav ! A.class_ "bg-near-white pa4 db w-100 mw8 br1" $
            H.div ! A.class_ "flex flex-wrap nl3 nr3" $ do
              H.div ! A.class_ "w-100 w-25-m w-25-l ph3" $ do
                H.h3 ! A.class_ "f5 ttu mv1" $ "Column 1"
                H.ul ! A.class_ "list pl0 mb3 mt0" $
                  links
              H.div ! A.class_ "w-100 w-25-m w-25-l ph3" $ do
                H.h3 ! A.class_ "f5 ttu mv1" $ "Column 2"
                H.ul ! A.class_ "list pl0 mb3 mt0" $
                  links
              H.div ! A.class_ "w-100 w-25-m w-25-l ph3" $ do
                H.h3 ! A.class_ "f5 ttu mv1" $ "Column 3"
                H.ul ! A.class_ "list pl0 mb3 mt0" $
                  links
              H.div ! A.class_ "w-100 w-25-m w-25-l ph3" $ do
                H.h3 ! A.class_ "f5 ttu mv1" $ "Column 4 (Section 1)"
                H.ul ! A.class_ "list pl0 mb3 mt0" $
                  links
                H.h3 ! A.class_ "f5 ttu mv1" $ "Column 4 (Section 2)"
                H.ul ! A.class_ "list pl0 mb3 mt0" $ do
                  H.li $
                    H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item One"
                  H.li $
                    H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item Two"
                  H.li $
                    H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item Three"
    H.footer $ do
      H.hr ! A.class_ "bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4 bw1 b--black"
      H.p ! A.class_ "inline-flex lh-copy" $ "© Hypered, 2019-2023."
 where
  links = do
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item One"
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item Two"
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item Three"
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item Four"
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item Five"

layoutWithSidebar :: Html
layoutWithSidebar =
  wrapper $ do
    H.div $ do
      H.header $
        H.nav ! A.class_ "flex justify-between align-items-center lh-copy mb4 pv3" $
          H.div $ do
            H.a ! A.class_ "black hy-hover-blue underline mr3" ! A.href "#" $ "noteed.com"
            H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "blog"
            H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "not-os"
      H.main ! A.class_ "flex flex-wrap nl3 nr3" $ do
        H.aside ! A.class_ "order-2 order-0-m order-0-l w-100 w-20-m w-20-l ph3 mt2" $
          H.nav $ do
            H.h3 ! A.class_ "f5 ttu mv1" $ "Intro"
            H.ul ! A.class_ "list pl0 mb3 mt0" $
              H.li $
                H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "not-os"
            H.h3 ! A.class_ "f5 ttu mv1" $ "Notes"
            H.ul ! A.class_ "list pl0 mb3 mt0" $ do
              H.li $
                H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Digital Ocean"
              H.li $
                H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "TODO"
            H.h3 ! A.class_ "f5 ttu mv1" $ "Values"
            H.ul ! A.class_ "list pl0 mb3 mt0" $ do
              H.li $
                H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "command-line"
              H.li $
                H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "root-modules"
        H.article ! A.class_ "order-0 order-1-m order-1-l w-100 m-60-m w-60-l ph3 cf" $ do
          H.div ! A.class_ "mb4" $ do
            h1 "Starting with NixOps (and thus Nix and NixOS)"
            H.hr ! A.class_ "mt3 pb3 bt-0 bl-0 br-0 bb b--black"
          h2 "Introduction"
          p $ "Given the three above derivations, it is possible to generate the appropriate qemu-kvm invocation as a script, runvm. runvm is the main entry point to start playing and understanding not-os. Follow the link, and enjoy!"
          H.ul ! A.class_ "hy-ff-tab-num mv3" $ do
            H.li ! A.class_ "mv1" $ "a kernel (config.system.build.kernel)"
            H.li ! A.class_ "mv1" $ "an initrd (config.system.build.initialRamdisk)"
            H.li ! A.class_ "mv1" $ "a rootfs (config.system.build.squashfs)"
          p $ "This is a project of Michael Bishop (cleverca22 on GitHub, clever on IRC). I modified it just a bit to make it possible to generate this documentation."
          h3 "Sub-points"
          H.ol ! A.class_ "hy-ff-tab-num mv3" $ do
            H.li ! A.class_ "mv1" $ "Item one"
            H.li ! A.class_ "mv1" $ "Item two"
            H.li ! A.class_ "mv1" $ "Item three"
          h4 "Some quotes"
          H.blockquote ! A.class_ "db bl bw2 pv2 ph3 ml0 mv4 lh-copy" $
            H.span ! A.class_ "i" $ "To follow along, you can clone the Git repository and run each nix-build command as they appear at the top of each page."
          h3 "Ending points"
          H.blockquote ! A.class_ "pull-quote relative db pv3 ph4 f4 ml0 mv4 lh-copy" $
            H.span ! A.class_ "i" $ "To follow along, you can clone the Git repository and run each nix-build command as they appear at the top of each page."
          p $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
        sidePanelExample
    H.footer $ do
      H.hr ! A.class_ "bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4 bw1 b--black"
      H.p ! A.class_ "inline-flex lh-copy" $ "© Hypered, 2019-2023."

--------------------------------------------------------------------------------
listOrderedExample :: Html
listOrderedExample =
  H.ol ! A.class_ "hy-ff-tab-num mv3" $ do
    H.li ! A.class_ "mv1" $ "Apple"
    H.li ! A.class_ "mv1" $ "Banana"
    H.li ! A.class_ "mv1" $ "Cherry"
    H.li ! A.class_ "mv1" $ "Durian"

listUnorderedExample :: Html
listUnorderedExample =
  H.ul ! A.class_ "hy-ff-tab-num mv3" $ do
    H.li ! A.class_ "mv1" $ "Apple"
    H.li ! A.class_ "mv1" $ "Banana"
    H.li ! A.class_ "mv1" $ "Cherry"
    H.li ! A.class_ "mv1" $ "Durian"


--------------------------------------------------------------------------------
modalTextContent :: Html
modalTextContent = do
  H.label ! A.class_ "bg-black b--black white hover-light-green ph4 pv3 pointer inline-flex button-reset ba bw1 relative"
          ! customAttribute "as" "label"
          ! A.for "modal"
          ! customAttribute "variant" "primary"
          $ "Open Modal"
  H.input ! A.type_ "checkbox" ! A.class_ "modal-state" ! A.id "modal"
  H.div ! A.class_ "modal items-center justify-center h-100 absolute absolute--fill z-1" $ do
    H.div ! A.class_ "bg-white relative z-2 mw6-m mw6-l center w-100 w-75-m w-50-l mh-75" $ do
      H.div ! A.class_ "flex items-center justify-between ba b--black bw1 pa3 b" $ do
        H.div $
          h4
            "Terms and Conditions"
        H.label ! A.for "modal" ! A.class_ "w2 h2 bg-white hover-bg-black-10 br1 flex items-center justify-center pointer" $
          H.img ! A.src "img/close.svg"
      H.div ! A.class_ "bg-white modal-body bl br b--black bw1 pa3 overflow-y-scroll" $
        H.article $ do
          H.h4 "1. YOUR AGREEMENT"
          H.p "By using this Site, you agree to be bound by, and to comply with, these Terms and Conditions. If you do not agree to these Terms and Conditions, please do not use this site. PLEASE NOTE: We reserve the right, at our sole discretion, to change, modify or otherwise alter these Terms and Conditions at any time. Unless otherwise indicated, amendments will become effective immediately. Please review these Terms and Conditions periodically. Your continued use of the Site following the posting of changes and/or modifications will constitute your acceptance of the revised Terms and Conditions and the reasonableness of these standards for notice of changes. For your information, this page was last updated as of the date at the top of these terms and conditions."
          H.h4 "2. PRIVACY"
          H.p "Please review our Privacy Policy, which also governs your visit to this Site, to understand our practices."
          H.h4 "3. LINKED SITES"
          H.p "This Site may contain links to other independent third-party Web sites (\"Linked Sites\"). These Linked Sites are provided solely as a convenience to our visitors. Such Linked Sites are not under our control, and we are not responsible for and does not endorse the content of such Linked Sites, including any information or materials contained on such Linked Sites. You will need to make your own independent judgment regarding your interaction with these Linked Sites."
          H.h4 "4. FORWARD LOOKING STATEMENTS"
          H.p "All materials reproduced on this site speak as of the original date of publication or filing. The fact that a document is available on this site does not mean that the information contained in such document has not been modified or superseded by events or by a subsequent document or filing. We have no duty or policy to update any information or statements contained on this site and, therefore, such information or statements should not be relied upon as being current as of the date you access this site."
          H.h4 "5. DISCLAIMER OF WARRANTIES AND LIMITATION OF LIABILITY"
          H.p "A. THIS SITE MAY CONTAIN INACCURACIES AND TYPOGRAPHICAL ERRORS. WE DOES NOT WARRANT THE ACCURACY OR COMPLETENESS OF THE MATERIALS OR THE RELIABILITY OF ANY ADVICE, OPINION, STATEMENT OR OTHER INFORMATION DISPLAYED OR DISTRIBUTED THROUGH THE SITE. YOU EXPRESSLY UNDERSTAND AND AGREE THAT: (i) YOUR USE OF THE SITE, INCLUDING ANY RELIANCE ON ANY SUCH OPINION, ADVICE, STATEMENT, MEMORANDUM, OR INFORMATION CONTAINED HEREIN, SHALL BE AT YOUR SOLE RISK; (ii) THE SITE IS PROVIDED ON AN \"AS IS\" AND \"AS AVAILABLE\" BASIS; (iii) EXCEPT AS EXPRESSLY PROVIDED HEREIN WE DISCLAIM ALL WARRANTIES OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING, BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, WORKMANLIKE EFFORT, TITLE AND NON-INFRINGEMENT; (iv) WE MAKE NO WARRANTY WITH RESPECT TO THE RESULTS THAT MAY BE OBTAINED FROM THIS SITE, THE PRODUCTS OR SERVICES ADVERTISED OR OFFERED OR MERCHANTS INVOLVED; (v) ANY MATERIAL DOWNLOADED OR OTHERWISE OBTAINED THROUGH THE USE OF THE SITE IS DONE AT YOUR OWN DISCRETION AND RISK; and (vi) YOU WILL BE SOLELY RESPONSIBLE FOR ANY DAMAGE TO YOUR COMPUTER SYSTEM OR FOR ANY LOSS OF DATA THAT RESULTS FROM THE DOWNLOAD OF ANY SUCH MATERIAL. B. YOU UNDERSTAND AND AGREE THAT UNDER NO CIRCUMSTANCES, INCLUDING, BUT NOT LIMITED TO, NEGLIGENCE, SHALL WE BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, PUNITIVE OR CONSEQUENTIAL DAMAGES THAT RESULT FROM THE USE OF, OR THE INABILITY TO USE, ANY OF OUR SITES OR MATERIALS OR FUNCTIONS ON ANY SUCH SITE, EVEN IF WE HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES. THE FOREGOING LIMITATIONS SHALL APPLY NOTWITHSTANDING ANY FAILURE OF ESSENTIAL PURPOSE OF ANY LIMITED REMEDY."
      H.div ! A.class_ "bg-blue flex justify-between" $ do
        H.label ! A.class_ "bg-white hover-bg-light-gray b--black black ph3 pb4 pt3 tl w-100 pointer inline-flex button-reset ba bw1 relative"
                ! customAttribute "as" "label"
                ! A.for "modal"
                ! customAttribute "variant" "secondary"
                $ "Dismiss"
        H.label ! A.class_ "bg-black b--black white hover-light-green ph3 pb4 pt3 tl w-100 pointer inline-flex button-reset ba bw1 relative"
                ! customAttribute "as" "label"
                ! A.for "modal"
                ! customAttribute "variant" "primary"
                $ "Accept and Continue —>"
    H.label ! A.class_ "bg-black-50 fixed absolute--fill z-1"
            ! A.for "modal" $ mempty

modalButtonLabel :: Html
modalButtonLabel = do
  H.label ! A.class_ "bg-black b--black white hover-light-green ph4 pv3 pointer inline-flex button-reset ba bw1 relative"
          ! customAttribute "as" "label"
          ! A.for "modal"
          ! customAttribute "variant" "primary"
          $ "Open Modal"
  modalContent

modalTextLabel :: Html
modalTextLabel = do
  H.label ! customAttribute "as" "label" ! A.for "modal" $ "Click on this text to open a modal"
  modalContent

modalContent :: Html
modalContent = do
  H.input ! A.type_ "checkbox" ! A.class_ "modal-state" ! A.id "modal"
  H.div ! A.class_ "modal items-center justify-center h-100 absolute absolute--fill z-1" $ do
    H.div ! A.class_ "bg-white relative z-2 mw6-m mw6-l center w-100 w-75-m w-50-l mh-75" $ do
      H.form $ do
        H.div ! A.class_ "flex items-center justify-between ba b--black bw1 pa3 b" $ do
          H.div $
            h4 "Log in to your account"
          H.label ! A.for "modal" ! A.class_ "w2 h2 bg-white hover-bg-black-10 br1 flex items-center justify-center pointer" $
            H.img ! A.src "img/close.svg"
        H.div ! A.class_ "bg-white modal-body bl br b--black bw1 pa3 overflow-y-scroll" $ do
          H.div ! A.class_ "mv3" $
            H.div ! A.class_ "mb3" $ do
              H.label ! A.class_ "db fw6 mv1" $ "Username"
              H.input ! A.type_ "text"
                      ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv3 ph2 w-100 outline-0 border-box"
                      ! A.name "username"
                      ! A.id "username"
              H.div ! A.class_ "mv1 h1 red fw5" $ "You have entered an invalid email"
          H.div ! A.class_ "mv3" $
            H.div ! A.class_ "mb3" $ do
              H.label ! A.class_ "db fw6 mv1" $ "Password"
              H.input ! A.type_ "password"
                      ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv3 ph2 w-100 outline-0 border-box"
                      ! A.placeholder ""
                      ! A.name "password"
                      ! A.id "password"
              H.div ! A.class_ "mv1 h1 red fw5" $ mempty
          H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Reset password"
        H.div ! A.class_ "bg-blue flex justify-between" $ do
          H.label ! A.class_ "bg-white hover-bg-light-gray b--black black ph3 pb4 pt3 tl w-100 pointer inline-flex button-reset ba bw1 relative"
                  ! customAttribute "as" "label"
                  ! A.for "modal"
                  ! customAttribute "variant" "secondary"
                  $ "Sign Up"
          H.label ! A.class_ "bg-black b--black white hover-light-green ph3 pb4 pt3 tl w-100 pointer inline-flex button-reset ba bw1 relative"
                  ! customAttribute "as" "label"
                  ! A.for "modal"
                  ! customAttribute "variant" "primary"
                  $ "Log In —>"
    H.label ! A.class_ "bg-black-50 fixed absolute--fill z-1"
            ! A.for "modal"
            $ mempty


------------------------------------------------------------------------------
navigationBlockDefault :: Html
navigationBlockDefault =
  H.nav ! A.class_ "bg-near-white pa4 db w-100 mw8 br1" $
    H.div ! A.class_ "flex flex-wrap nl3 nr3" $ do
      H.div ! A.class_ "w-100 w-25-m w-25-l ph3" $ do
        H.h3 ! A.class_ "f5 ttu mv1" $ "Column 1"
        H.ul ! A.class_ "list pl0 mb3 mt0" $
          links
      H.div ! A.class_ "w-100 w-25-m w-25-l ph3" $ do
        H.h3 ! A.class_ "f5 ttu mv1" $ "Column 2"
        H.ul ! A.class_ "list pl0 mb3 mt0" $
          links
      H.div ! A.class_ "w-100 w-25-m w-25-l ph3" $ do
        H.h3 ! A.class_ "f5 ttu mv1" $ "Column 3"
        H.ul ! A.class_ "list pl0 mb3 mt0" $
          links
      H.div ! A.class_ "w-100 w-25-m w-25-l ph3" $ do
        H.h3 ! A.class_ "f5 ttu mv1" $ "Column 4 (Section 1)"
        H.ul ! A.class_ "list pl0 mb3 mt0" $
          links
        H.h3 ! A.class_ "f5 ttu mv1" $ "Column 4 (Section 2)"
        H.ul ! A.class_ "list pl0 mb3 mt0" $ do
          H.li $
            H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item One"
          H.li $
            H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item Two"
          H.li $
            H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item Three"
 where
  links = do
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item One"
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item Two"
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item Three"
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item Four"
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href "#" $ "Item Five"

navigationBlockUsage :: Html
navigationBlockUsage =
  wrapper $ do
    H.div $ do
      H.header $
        navigationNoteedX
      H.div ! A.class_ "flex flex-wrap nl3 nr3" $ do
        sidebar
          [ ("Intro", [("not-os", "#")])
          , ("Notes", [("Digital Ocean", "#"), ("TODO", "#")])
          , ("Values", [("command-line", "#"), ("root-modules", "#")])
          ]
        H.main ! A.class_ "w-100 w-75-m w-75-l ph3" $
          H.article $ do
            h1 "Heading 1"
            p
              "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque eu magna pharetra, ultricies nisi eget, aliquet nulla. Curabitur in ultricies diam. In auctor neque ante, at vulputate magna volutpat nec. Morbi pharetra metus vitae dignissim tincidunt. Nunc iaculis consectetur purus, quis egestas orci gravida pulvinar. Quisque sed malesuada arcu. Sed nisl mi, lobortis eget diam in, facilisis tincidunt mi. Sed et neque laoreet, placerat risus ac, volutpat ex. Suspendisse venenatis lectus id turpis congue, a interdum est viverra. Etiam et nunc et mauris mattis pulvinar. Aliquam mattis lacinia molestie. Maecenas molestie ornare sollicitudin."
            p
              "Mauris pretium velit eget turpis rhoncus volutpat. Suspendisse sit amet egestas lorem. Cras posuere ac tortor vel interdum. Mauris ultrices euismod dui ac placerat. Aliquam aliquet erat id mauris placerat fermentum. Pellentesque ut sodales ipsum. Donec gravida, dui at tempus pretium, dolor urna aliquet est, mattis pellentesque odio elit a massa. Morbi ut efficitur tortor. Pellentesque ut dolor et augue mollis accumsan et sit amet urna. Mauris ac eros non mauris sodales posuere. Nunc a tortor eget quam laoreet suscipit. Phasellus sollicitudin suscipit libero."
            h2 "Heading 2"
            p
              "Praesent vel hendrerit risus, nec sagittis enim. Cras nec lobortis justo. Praesent accumsan turpis scelerisque, fermentum metus sed, volutpat lectus. Mauris tempus eget ex sit amet vestibulum. Aliquam ut enim commodo, gravida odio in, venenatis est. Maecenas iaculis blandit tincidunt. Aenean faucibus luctus tellus, eu aliquet justo sollicitudin a. Donec eu convallis urna, quis porta quam. Vivamus rutrum et odio in varius. Sed tristique auctor mi, non vehicula est vehicula ut. Aliquam vestibulum, odio id finibus euismod, mauris ipsum iaculis massa, vel feugiat turpis sapien nec orci."
      navigationBlockDefault
    footer "© Hypered, 2019-2023."


------------------------------------------------------------------------------
navigationDefault :: Html
navigationDefault =
  nav $
    H.div $ do
      H.a ! A.class_ "black hy-hover-blue underline mr3" ! A.href "#" $ "noteed.com"
      H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "blog"
      H.a ! A.class_ "black hy-hover-blue" ! A.href "#" $ "not-os"

navigationSpaceBetween :: Html
navigationSpaceBetween =
  H.nav ! A.class_ "flex justify-between align-items-center lh-copy mb4 pv3" $ do
    H.div $
      H.a ! A.class_ "black hy-hover-blue underline mr3" ! A.href "#" $
        "noteed.com"
    H.div $ do
      H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "blog"
      H.a ! A.class_ "black hy-hover-blue" ! A.href "#" $ "not-os"


--------------------------------------------------------------------------------
headTitle :: Html
headTitle = H.title "Hypered"

sidebarTitle :: Html -> Html
sidebarTitle content =
  H.h3 ! A.class_ "f5 ttu mv1" $ content

sidebarUL :: Html -> Html
sidebarUL content =
  H.ul ! A.class_ "list pl0 mb3 mt0" $ content

sidebarLI :: Html -> Html
sidebarLI content =
  H.li content

sidebarLink :: H.AttributeValue -> Text -> Html
sidebarLink href content =
  H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href href $ H.text content

sidebar :: [(Html, [(Text, H.AttributeValue)])] -> Html
sidebar xs =
  H.aside ! A.class_ "order-2 order-0-m order-0-l w-100 w-20-m w-20-l ph3 mt2" $
    H.nav $ do
      mapM_ f xs

  where

  f (title, links) = do
    sidebarTitle title
    sidebarUL $
      mapM_ g links
  g (name, href) =
    sidebarLI $
      sidebarLink href name

exampleLoginForm :: Html
exampleLoginForm = do
  wrapper $ do
    H.div $ do
      H.header $
        navigationReesd
      H.p "Reesd is in private alpha. New registrations are currently disabled."
      loginForm
    -- There could be a footer, but on simple forms, I think I prefer without.
    -- footer "© Hypered, 2020-2023."

exampleLoginFormReesd :: Html
exampleLoginFormReesd = do
  wrapper $ do
    H.div $ do
      H.header $
        navigationReesd
      H.p "Reesd is in private alpha. New registrations are currently disabled."
      loginFormReesd
    -- There could be a footer, but on simple forms, I think I prefer without.
    -- footer "© Hypered, 2020-2023."

exampleRegisterForm :: Html
exampleRegisterForm = do
  wrapper $ do
    H.div $ do
      H.header $
        navigationReesd
      H.p "Reesd is in private alpha. New registrations are currently disabled."
      registerForm
    -- There could be a footer, but on simple forms, I think I prefer without.
    -- footer "© Hypered, 2020-2023."

exampleResetForm :: Html
exampleResetForm = do
  wrapper $ do
    H.div $ do
      H.header $
        navigationReesd
      H.p "Enter a verified email address and we'll send a password reset link\
        \ to that address."
      resetForm
    -- There could be a footer, but on simple forms, I think I prefer without.
    -- footer "© Hypered, 2020-2023."

exampleResetFormReesd :: Html
exampleResetFormReesd = do
  wrapper $ do
    H.div $ do
      H.header $
        navigationReesd
      H.p "Enter a verified email address and we'll send a password reset link\
        \ to that address."
      resetFormReesd
    -- There could be a footer, but on simple forms, I think I prefer without.
    -- footer "© Hypered, 2020-2023."

exampleSidebar :: Html
exampleSidebar =
  wrapper $ do
    H.div $ do
      H.header $
        navigationNoteedX
      H.main ! A.class_ "flex flex-wrap nl3 nr3" $ do
        sidebar
          [ ("Intro", [("not-os", "#")])
          , ("Notes", [("Digital Ocean", "#"), ("TODO", "#")])
          , ("Values", [("command-line", "#"), ("root-modules", "#")])
          ]
        H.section ! A.class_ "order-0 order-1-m order-1-l w-100 w-75-m w-75-l ph3" $
          H.article $ do
            h1 "not-os"
            p $ do
              "not-os is a minimal OS based on the Linux kernel, coreutils, "
              "runit, and Nix. It is also the build script, written in Nix "
              "expressions, to build such OS."
            p $ do
              "This is a project of Michael Bishop (cleverca22 on GitHub, clever on "
              "IRC). I modified it just a bit to make it possible to generate this "
              "documentation."
            p $ do
              "As a build tool, not-os uses nixpkgs and in particular the "
              H.a ! A.href "https://nixos.wiki/wiki/NixOS_Modules" $ "NixOS module system"
              " to build the three main components of a Linux-based operating "
              "system:"
    footer "© Hypered, 2019-2023."

exampleSidePanel :: Html
exampleSidePanel = do
  wrapper $ do
    H.div $ do
      H.header $
        navigationNoteedX
      -- H.main $
      identity $ -- TODO I think H.main should be present.
        H.div ! A.class_ "flex flex-wrap nl3 nr3" $ do
          H.main ! A.class_ "w-100 w-80-m w-80-l ph3" $
            H.article $ do
              h1 "Waveguide"
              p $ do
                "If neither a list of attribute names or a command are given, "
                "Waveguide instrospects the Nix expression and builds all the "
                "found attributes."
          sidePanelExample
    footer "© Hypered, 2019-2023."

sidePanelLink :: H.AttributeValue -> Text -> Html
sidePanelLink href content =
  H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href href $ H.text content


------------------------------------------------------------------------------
-- Forms
------------------------------------------------------------------------------

-- | Register form
registerForm :: Html
registerForm = do
  formPost "/a/register" $ do
    formBody "Register for Reesd" $ do
      H.div ! A.class_ "mv3" $
        H.div ! A.class_ "mb3" $ do
          H.label ! A.class_ "db fw6 mv1" $ "Username"
                  ! A.for "username"
          H.input ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv2 ph2 w-100 outline-0 border-box"
                  ! A.label "username"
                  ! A.name "username"
                  ! A.id "username"
                  ! A.type_ "text"
                  ! A.placeholder ""
      H.div ! A.class_ "mv3" $
        H.div ! A.class_ "mb3" $ do
          H.label ! A.class_ "db fw6 mv1" $ "Email address"
                  ! A.for "email"
          H.input ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv2 ph2 w-100 outline-0 border-box"
                  ! A.label "email"
                  ! A.name "email"
                  ! A.id "email"
                  ! A.type_ "email"
                  ! A.placeholder ""
          -- H.div ! A.class_ "mv1 h1 red fw5" $ You have entered an invalid email
      H.div ! A.class_ "mv3" $
        H.div ! A.class_ "mb3" $ do
          H.label ! A.class_ "db fw6 mv1" $ "Password"
                  ! A.for "password"
          H.input ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv2 ph2 w-100 outline-0 border-box"
                  ! A.label "password"
                  ! A.name "password"
                  ! A.id "password"
                  ! A.type_ "password"
                  ! A.placeholder ""
          -- H.div ! A.class_ "mv1 h1 red fw5" $ ""
    formButtons $ do
      formButtonLink "/login" "Log in"
      formButton "Register —>"

-- | Reset form
resetForm :: Html
resetForm = do
  formPost "/a/reset" $ do
    formBody "Reset password for Reesd" $
      H.div ! A.class_ "mv3" $
        H.div ! A.class_ "mb3" $ do
          H.label ! A.class_ "db fw6 mv1" $ "Email address"
                  ! A.for "email"
          H.input ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv3 ph2 w-100 outline-0 border-box"
                  ! A.label "email"
                  ! A.name "email"
                  ! A.id "email"
                  ! A.type_ "email"
                  ! A.placeholder ""
          -- H.div ! A.class_ "mv1 h1 red fw5" $ You have entered an invalid email
    formButtons $ do
      formButtonLink "/login" "Log in"
      formButton "Reset password —>"

-- | Reset form
resetFormReesd :: Html
resetFormReesd = do
  formPost "/a/reset" $ do
    formBody "Reset password for Reesd" $
      formFieldEmailSmall "email" "Email address" Nothing
    formButtons $ do
      formButtonLink "/login" "Log in"
      formButton "Reset password —>"


------------------------------------------------------------------------------
radioDefaultExample :: Html
radioDefaultExample =
  H.div ! A.class_ "db" $ do
    H.label ! A.class_ "flex items-center mb2 flex" $ do
      H.input ! A.type_ "radio"
              ! A.class_ "hy-checkbox w1 h1 br-pill"
              ! A.name "fruits"
              ! A.checked ""
              ! A.value "apple"
      H.div ! A.class_ "ml1" $ "Apple"
    H.label ! A.class_ "flex items-center mb2 flex" $ do
      H.input ! A.type_ "radio"
              ! A.class_ "hy-checkbox w1 h1 br-pill"
              ! A.name "fruits"
              ! A.value "banana"
      H.div ! A.class_ "ml1" $ "Banana"
    H.label ! A.class_ "flex items-center mb2 flex" $ do
      H.input ! A.type_"radio"
              ! A.class_ "hy-checkbox w1 h1 br-pill"
              ! A.name "fruits"
              ! A.value "cherry"
      H.div ! A.class_ "ml1" $ "Cherry"

radioPillInlineExample :: Html
radioPillInlineExample =
  H.div ! A.class_ "flex items-center" $ do
    H.label ! A.class_ "flex items-center mr3 inline-flex" $ do
      H.input ! A.type_ "radio"
              ! A.class_ "hy-checkbox w1 h1 br-pill"
              ! A.name "fruits"
              ! A.checked ""
              ! A.value "apple"
      H.div ! A.class_ "ml1" $ "Apple"
    H.label ! A.class_ "flex items-center mr3 inline-flex" $ do
      H.input ! A.type_ "radio"
              ! A.class_ "hy-checkbox w1 h1 br-pill"
              ! A.name "fruits"
              ! A.value "banana"
      H.div ! A.class_ "ml1" $ "Banana"
    H.label ! A.class_ "flex items-center mr3 inline-flex" $ do
      H.input ! A.type_ "radio"
              ! A.class_ "hy-checkbox w1 h1 br-pill"
              ! A.name "fruits"
              ! A.value "cherry"
      H.div ! A.class_ "ml1" $ "Cherry"

radioCheckboxExample :: Html
radioCheckboxExample =
  H.div ! A.class_ "db" $ do
    H.label ! A.class_ "flex items-center mb2 flex" $ do
      H.input ! A.type_ "radio"
              ! A.class_ "hy-checkbox w1 h1"
              ! A.name "fruits"
              ! A.checked ""
              ! A.value "apple"
      H.div ! A.class_ "ml1" $ "Apple"
    H.label ! A.class_ "flex items-center mb2 flex" $ do
      H.input ! A.type_ "radio"
              ! A.class_ "hy-checkbox w1 h1"
              ! A.name "fruits"
              ! A.value "banana"
      H.div ! A.class_ "ml1" $ "Banana"
    H.label ! A.class_ "flex items-center mb2 flex" $ do
      H.input ! A.type_ "radio"
              ! A.class_ "hy-checkbox w1 h1"
              ! A.name "fruits"
              ! A.value "cherry"
      H.div ! A.class_ "ml1" $ "Cherry"

radioCheckboxInlineExample :: Html
radioCheckboxInlineExample =
  H.div ! A.class_ "flex items-center" $ do
    H.label ! A.class_ "flex items-center mr3 inline-flex" $ do
      H.input ! A.type_ "radio"
              ! A.class_ "hy-checkbox w1 h1"
              ! A.name "fruits"
              ! A.checked ""
              ! A.value "apple"
      H.div ! A.class_ "ml1" $ "Apple"
    H.label ! A.class_ "flex items-center mr3 inline-flex" $ do
      H.input ! A.type_ "radio"
              ! A.class_ "hy-checkbox w1 h1"
              ! A.name "fruits"
              ! A.value "banana"
      H.div ! A.class_ "ml1" $ "Banana"
    H.label ! A.class_ "flex items-center mr3 inline-flex" $ do
      H.input ! A.type_ "radio"
              ! A.class_ "hy-checkbox w1 h1"
              ! A.name "fruits"
              ! A.value "cherry"
      H.div ! A.class_ "ml1" $ "Cherry"


------------------------------------------------------------------------------
sidePanelExample :: Html
sidePanelExample =
  H.aside ! A.class_ "order-1 order-2-m order-2-l w-100 w-20-m w-20-l ph3 mt2" $
    H.div ! A.class_ "" $ do
      H.h3 ! A.class_ "f5 lh-title mv2" $ "Latest Runs"
      H.ul ! A.class_ "bg-near-white list pa3" $ do
        mapM_ ((H.li ! A.class_ "pv1 bb b--black-10") . sidePanelLink "#")
          [ "→ #001"
          , "→ #002"
          , "→ #003"
          , "→ #004"
          , "→ #005"
          ]

sidePanelUsageExample :: Html
sidePanelUsageExample = exampleSidePanel


------------------------------------------------------------------------------
sidebarExample :: Html
sidebarExample =
    sidebar
      [ ("Intro", [("not-os", "#")])
      , ("Notes", [("Digital Ocean", "#"), ("TODO", "#")])
      , ("Values", [("command-line", "#"), ("root-modules", "#")])
      ]

sidebarUsageExample :: Html
sidebarUsageExample = exampleSidebar


------------------------------------------------------------------------------
statusCodeError400Example :: Html
statusCodeError400Example =
  H.div ! A.class_ "flex items-center justify-center vh-100 mw8 center pa4" $
    H.div $ do
      H.label ! A.class_ "dib bg-red white f6 mv0 pv1 ph2" $ "Status"
      H.div ! A.class_ "ba b--red debug-grid-16" $
        H.div ! A.class_ "pa6" $ do
          H.h2 ! A.class_ "red f4 fw8 tracked-tight lh-title mv0 ttu" $ "Error"
          H.h3 ! A.class_ "glitch f1 f-subheadline-m f-subheadline-l fw9 tracked-tight lh-title mv0"
               ! customAttribute "data-text" "400 Bad Gateway"
               $ "400 Bad Gateway"
          H.div $
            H.div $ do
              p $
                "Looks like the page you're looking for is unavailable. You can click here to return to the home page, or visit any of the links below:"
              H.ul ! A.class_ "hy-ff-tab-num mv3" $ do
                H.li ! A.class_ "mv1" $
                  H.a ! A.class_ "no-underline hy-hover-blue"
                      ! A.href "/"
                      $ "Home"
                H.li ! A.class_ "mv1" $
                  H.a ! A.class_ "no-underline hy-hover-blue"
                      ! A.href "components/"
                      $ "Components"
                H.li ! A.class_ "mv1" $
                  H.a ! A.class_ "no-underline hy-hover-blue"
                      ! A.href "storybook/"
                      $ "Storybook"
                H.li ! A.class_ "mv1" $
                  H.a ! A.class_ "no-underline hy-hover-blue"
                      ! A.href "documentation/"
                      $ "Documentation"
                H.li ! A.class_ "mv1" $
                  H.a ! A.class_ "no-underline hy-hover-blue"
                      ! A.href "haddock/"
                      $ "Haddock"

statusCodeError404Example :: Html
statusCodeError404Example =
  H.div ! A.class_ "flex items-center justify-center vh-100 mw8 center pa4" $
    H.div $ do
      H.label ! A.class_ "dib bg-red white f6 mv0 pv1 ph2" $ "Status"
      H.div ! A.class_ "ba b--red debug-grid-16" $
        H.div ! A.class_ "pa6" $ do
          H.h2 ! A.class_ "red f4 fw8 tracked-tight lh-title mv0 ttu" $ "Error"
          H.h3 ! A.class_ "glitch f1 f-subheadline-m f-subheadline-l fw9 tracked-tight lh-title mv0"
               ! customAttribute "data-text" "404 Not Found"
               $ "404 Not Found"
          H.div $
            H.div $ do
              p $
                "Looks like the page you're looking for is unavailable. You can click here to return to the home page, or visit any of the links below:"
              H.ul ! A.class_ "hy-ff-tab-num mv3" $ do
                H.li ! A.class_ "mv1" $
                  H.a ! A.class_ "no-underline hy-hover-blue"
                      ! A.href "/"
                      $ "Home"
                H.li ! A.class_ "mv1" $
                  H.a ! A.class_ "no-underline hy-hover-blue"
                      ! A.href "components/"
                      $ "Components"
                H.li ! A.class_ "mv1" $
                  H.a ! A.class_ "no-underline hy-hover-blue"
                      ! A.href "storybook/"
                      $ "Storybook"
                H.li ! A.class_ "mv1" $
                  H.a ! A.class_ "no-underline hy-hover-blue"
                      ! A.href "documentation/"
                      $ "Documentation"
                H.li ! A.class_ "mv1" $
                  H.a ! A.class_ "no-underline hy-hover-blue"
                      ! A.href "haddock/"
                      $ "Haddock"


------------------------------------------------------------------------------
tableDefault :: Html
tableDefault =
  H.div ! A.class_ "overflow-x-scroll" $
    H.table ! A.class_ "bg-white collapse w-100" $ do
      H.thead $
        H.tr ! A.class_ "b--black bw1 bb" $ do
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl" $ "Column 1"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl" $ "Column 2"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl" $ "Column 3"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl" $ "Column 4"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tr"
               ! customAttribute "align" "right" $
            "Column 5"
      H.tbody $
        replicateM_ 10 $
          H.tr ! A.class_ "b--black bb" $ do
            H.td ! A.class_ "nowrap f5 pa2 tl" $ "Red"
            H.td ! A.class_ "nowrap f5 pa2 tl" $ "Green"
            H.td ! A.class_ "nowrap f5 pa2 tl" $ "Blue"
            H.td ! A.class_ "nowrap f5 pa2 tl" $ "Yellow"
            H.td ! A.class_ "nowrap f5 pa2 tr"
                 ! customAttribute "align" "right" $ "001"

tableCompact :: Html
tableCompact =
  H.div ! A.class_ "overflow-x-scroll" $
    H.table ! A.class_ "bg-white collapse w-100" $ do
      H.thead $
        H.tr ! A.class_ "b--black bw1 bb" $ do
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl" $ "Column 1"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl" $ "Column 2"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl" $ "Column 3"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl" $ "Column 4"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tr"
               ! customAttribute "align" "right" $
            "Column 5"
      H.tbody $
        replicateM_ 10 $
          H.tr ! A.class_ "b--black bb" $ do
            H.td ! A.class_ "nowrap f6 pa1 tl" $ "Red"
            H.td ! A.class_ "nowrap f6 pa1 tl" $ "Green"
            H.td ! A.class_ "nowrap f6 pa1 tl" $ "Blue"
            H.td ! A.class_ "nowrap f6 pa1 tl" $ "Yellow"
            H.td ! A.class_ "nowrap f6 pa1 tr"
                 ! customAttribute "align" "right" $ "001"

tableWithColumnDivider :: Html
tableWithColumnDivider =
  H.div ! A.class_ "overflow-x-scroll" $
    H.table ! A.class_ "bg-white collapse w-100" $ do
      H.thead $
        H.tr ! A.class_ "b--black bw1 bb" $ do
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl br" $ "Column 1"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl br" $ "Column 2"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl br" $ "Column 3"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl br" $ "Column 4"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tr"
               ! customAttribute "align" "right" $
            "Column 5"
      H.tbody $ do
        replicateM_ 9 $
          H.tr ! A.class_ "b--black bb" $ do
            H.td ! A.class_ "nowrap f5 pa2 tl br" $ "Red"
            H.td ! A.class_ "nowrap f5 pa2 tl br" $ "Green"
            H.td ! A.class_ "nowrap f5 pa2 tl br" $ "Blue"
            H.td ! A.class_ "nowrap f5 pa2 tl br" $ "Yellow"
            H.td ! A.class_ "nowrap f5 pa2 tr"
                 ! customAttribute "align" "right" $ "001"
        replicateM_ 1 $
          H.tr ! A.class_ "b--black" $ do
            H.td ! A.class_ "nowrap f5 pa2 tl br" $ "Red"
            H.td ! A.class_ "nowrap f5 pa2 tl br" $ "Green"
            H.td ! A.class_ "nowrap f5 pa2 tl br" $ "Blue"
            H.td ! A.class_ "nowrap f5 pa2 tl br" $ "Yellow"
            H.td ! A.class_ "nowrap f5 pa2 tr"
                 ! customAttribute "align" "right" $ "001"

tableWithColumnDividerCompact :: Html
tableWithColumnDividerCompact =
  H.div ! A.class_ "overflow-x-scroll" $
    H.table ! A.class_ "bg-white collapse w-100" $ do
      H.thead $
        H.tr ! A.class_ "b--black bw1 bb" $ do
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl br" $ "Column 1"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl br" $ "Column 2"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl br" $ "Column 3"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl br" $ "Column 4"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tr"
               ! customAttribute "align" "right" $
            "Column 5"
      H.tbody $ do
        replicateM_ 9 $
          H.tr ! A.class_ "b--black bb" $ do
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Red"
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Green"
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Blue"
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Yellow"
            H.td ! A.class_ "nowrap f6 pa1 tr"
                 ! customAttribute "align" "right" $ "001"
        replicateM_ 1 $
          H.tr ! A.class_ "b--black" $ do
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Red"
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Green"
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Blue"
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Yellow"
            H.td ! A.class_ "nowrap f6 pa1 tr"
                 ! customAttribute "align" "right" $ "001"


--------------------------------------------------------------------------------
titleJumboExample :: Html
titleJumboExample =
  H.h1 ! A.class_ "f1 f-subheadline-m f-subheadline-l tracked-tight mv2" $ do
    "Hypered "
    H.span ! A.class_ "normal" $ "design system"

titleSubtitleJumboExample :: Html
titleSubtitleJumboExample =
  H.h2 ! A.class_ "f1 tracked-tight mv2" $ "Introduction"

titleDefaultExample :: Html
titleDefaultExample =
  H.h1 ! A.class_ "f1 f1-l tracked-tight mv2" $ "Introduction"

titleSubtitleDefaultExample :: Html
titleSubtitleDefaultExample =
  H.h2 ! A.class_ "f2 tracked-tight mv2" $ "Introduction"

titleJumboUsageExample :: Html
titleJumboUsageExample =
  H.div ! A.class_ "mw8 center" $ do
    H.header ! A.class_ "pv4 pv5-l" $ do
      H.h1 ! A.class_ "f1 f-subheadline-m f-subheadline-l tracked-tight mv2" $
        "Hypered"
      H.h2 ! A.class_ "f1 tracked-tight mv2" $
        "Software development, defined"
      p $
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent convallis mollis nulla, molestie tempor velit consequat non."
    H.hr ! A.class_ "mt3 pb3 bt-0 bl-0 br-0 bb b--black"
    H.section ! A.class_ "pv4 pv5-l" $ do
      H.h2 ! A.class_ "f1 tracked-tight mv2" $ "Introduction"
      p $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent convallis mollis nulla, molestie tempor velit consequat non. Integer quam ligula, consequat eget semper in, sodales nec mauris. Sed ultrices enim quis eros lobortis, semper condimentum eros sodales. Morbi iaculis lectus id dui convallis feugiat."
    H.hr ! A.class_ "mt3 pb3 bt-0 bl-0 br-0 bb b--black"
    H.section ! A.class_ "pv4 pv5-l" $
      H.div ! A.class_ "flex flex-wrap nl3 nr3 tc" $ do
        H.div ! A.class_ "w-100 w-third-l ph3" $ do
          H.h2 ! A.class_ "f1 tracked-tight mv2" $ "23,000"
          p $ "downloads"
        H.div ! A.class_ "w-100 w-third-l ph3" $ do
          H.h2 ! A.class_ "f1 tracked-tight mv2" $ "3.2kb"
          p $ "gzipped"
        H.div ! A.class_ "w-100 w-third-l ph3" $ do
          H.h2 ! A.class_ "f1 tracked-tight mv2" $ "626"
          p $ "stars on GitHub"

titleDefaultUsageExample :: Html
titleDefaultUsageExample =
  H.div ! A.class_ "mw8 center" $ do
    H.header ! A.class_ "pv4 pv5-l" $ do
      H.h1 ! A.class_ "f1 f1-l tracked-tight mv2" $ "Hypered"
      H.h2 ! A.class_ "f2 tracked-tight mv2" $ "Software development, defined"
      p $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent convallis mollis nulla, molestie tempor velit consequat non."
    H.hr ! A.class_ "mt3 pb3 bt-0 bl-0 br-0 bb b--black"
    H.section ! A.class_ "pv4 pv5-l" $ do
      H.h2 ! A.class_ "f2 tracked-tight mv2" $ "Introduction"
      p $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent convallis mollis nulla, molestie tempor velit consequat non. Integer quam ligula, consequat eget semper in, sodales nec mauris. Sed ultrices enim quis eros lobortis, semper condimentum eros sodales. Morbi iaculis lectus id dui convallis feugiat."
    H.hr ! A.class_ "mt3 pb3 bt-0 bl-0 br-0 bb b--black"
    H.section ! A.class_ "pv4 pv5-l" $
      H.div ! A.class_ "flex flex-wrap nl3 nr3 tc" $ do
        H.div ! A.class_ "w-100 w-third-l ph3" $ do
          H.h2 ! A.class_ "f2 tracked-tight mv2" $ "23,000"
          p $ "downloads"
        H.div ! A.class_ "w-100 w-third-l ph3" $ do
          H.h2 ! A.class_ "f2 tracked-tight mv2" $ "3.2kb"
          p $ "gzipped"
        H.div ! A.class_ "w-100 w-third-l ph3" $ do
          H.h2 ! A.class_ "f2 tracked-tight mv2" $ "626"
          p $ "stars on GitHub"


--------------------------------------------------------------------------------
h1 :: Text -> Html
h1 content =
  H.h1 ! A.class_ "f1 lh-title mv2 tracked-tight" $ H.text content

h2 :: Text -> Html
h2 content =
  H.h2 ! A.class_ "f2 lh-title mv2 tracked-tight" $ H.text content

h3 :: Text -> Html
h3 content =
  H.h3 ! A.class_ "f3 lh-title mv2 tracked-tight" $ H.text content

h4 :: Text -> Html
h4 content =
  H.h4 ! A.class_ "f4 lh-title mv2 tracked-tight" $ H.text content

h5 :: Text -> Html
h5 content =
  H.h5 ! A.class_ "f5 lh-title mv2" $ H.text content

h6 :: Text -> Html
h6 content =
  H.h6 ! A.class_ "f6 lh-title mv2 ttu" $ H.text content

p :: Html -> Html
p content =
  H.p ! A.class_ "f5 lh-copy mv3" $ content


--------------------------------------------------------------------------------
typographyHeading1Example :: Html
typographyHeading1Example = h1 "Heading 1"

typographyHeading2Example :: Html
typographyHeading2Example = h2 "Heading 2"

typographyHeading3Example :: Html
typographyHeading3Example = h3 "Heading 3"

typographyHeading4Example :: Html
typographyHeading4Example = h4 "Heading 4"

typographyHeading5Example :: Html
typographyHeading5Example = h5 "Heading 5"

typographyHeading6Example :: Html
typographyHeading6Example = h6 "Heading 6"

typographyParagraphExample :: Html
typographyParagraphExample =
  p $
   "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla sollicitudin malesuada est. Sed efficitur laoreet massa, eu dictum est luctus sit amet. Morbi elementum dapibus pellentesque. Sed varius nisi nisi, nec imperdiet nunc bibendum at. Maecenas bibendum, neque nec vehicula dignissim, sem dolor congue risus, ac consequat sapien nibh in felis. Suspendisse at est a nisl dictum condimentum. Suspendisse ut dolor vitae nisi dictum hendrerit a vel magna. Etiam porttitor lacus magna, non bibendum tellus lobortis sit amet. Duis quis lectus massa. Nam quis fringilla dui. Fusce felis leo, iaculis id dui eu, lacinia varius dolor. Praesent molestie rhoncus mi, ac malesuada neque placerat vitae. Aliquam placerat auctor pretium. Mauris egestas condimentum erat sit amet tempor. Quisque imperdiet, augue nec eleifend placerat, lacus tortor pellentesque metus, sit amet laoreet lectus enim et mauris. Vivamus sollicitudin a ex sit amet ullamcorper."

typographyUsageExample :: Html
typographyUsageExample =
  H.article ! A.class_ "measure-wide" $ do
    h1 "Design System Blog"
    p "This is an intro to using Hypered design system."
    h2 "Components"
    p
      "Hypered design system comprises of components that quickly help you get started with your projects."
    p $ do
      "The components in this design system are built with "
      H.a ! A.class_ "hy-blue no-underline hy-hover-blue" ! A.href "#" $ "Tachyons"
      "."


--------------------------------------------------------------------------------
whitespaceAutoWidthExample :: Html
whitespaceAutoWidthExample =
  H.div ! A.class_ "flex justify-center mb4" $
    H.div ! A.class_ "h5 w-auto self-center" $
      H.div ! A.class_ "bg-navy light-green flex justify-between h5" $ do
        H.div ! A.class_ "flex flex-column justify-between pa4" $ do
          H.h2 ! A.class_ "f3 f2-m f2-l fw8 lh-title mv1" $ "01. Auto Width"
          H.div $ do
            H.p ! A.class_ "f7 b tracked lh-copy ttu mv1 o-50" $ "Description"
            H.p ! A.class_ "f5 lh-copy mv1" $
              "This box takes up the size of its content and is centered."
        H.b ! A.class_ "flex items-center justify-start pa3 bl f6"
            ! A.style "writing-mode:vertical-lr;text-orientation:mixed"
            $ "Hypered Design System"

whitespaceNegativeMarginsExample :: Html
whitespaceNegativeMarginsExample =
  H.div ! A.class_ "nl4 nr4 mb4" $
    H.div ! A.class_ "bg-navy light-green flex justify-between h5" $ do
      H.div ! A.class_ "flex flex-column justify-between pa4" $ do
        H.h2 ! A.class_ "f3 f2-m f2-l fw8 lh-title mv1" $
          "02. Negative Margins"
        H.div $ do
          H.p ! A.class_ "f7 b tracked lh-copy ttu mv1 o-50" $
            "Description"
          H.p ! A.class_ "f5 lh-copy mv1" $
            "This box has negative margins and should extend beyond its parent's container."
      H.b ! A.class_ "flex items-center justify-start pa3 bl f6"
          ! A.style "writing-mode:vertical-lr;text-orientation:mixed"
          $ "Hypered Design System"

whitespaceFullWidthExample :: Html
whitespaceFullWidthExample =
  H.div ! A.class_ "w-100" $
    H.div ! A.class_ "bg-navy light-green flex justify-between h5" $ do
      H.div ! A.class_ "flex flex-column justify-between pa4" $ do
        H.h2 ! A.class_ "f3 f2-m f2-l fw8 lh-title mv1" $ "03. Full Width"
        H.div $ do
          H.p ! A.class_ "f7 b tracked lh-copy ttu mv1 o-50" $
            "Description"
          H.p ! A.class_ "f5 lh-copy mv1" $
            "This box takes up the full width of its parent's container."
      H.b ! A.class_ "flex items-center justify-start pa3 bl f6"
          ! A.style "writing-mode:vertical-lr;text-orientation:mixed"
          $ "Hypered Design System"

whitespaceExamples :: Html
whitespaceExamples =
  wrapper $ do
    H.div $ do
      H.header $
        H.nav ! A.class_ "flex justify-between align-items-center lh-copy mb4 pv3" $
          H.div $ do
            H.a ! A.class_ "black hy-hover-blue underline mr3"
                ! A.href "#"
                $ "noteed.com"
            H.a ! A.class_ "black hy-hover-blue mr3"
                ! A.href "#"
                $ "blog"
            H.a ! A.class_ "black hy-hover-blue mr3"
                ! A.href "#"
                $ "not-os"
      H.div $ do
        H.label ! A.class_ "dib bg-red white f6 mv0 pv1 ph2" $ "Container"
        H.div ! A.class_ "ba b--red debug-grid-16" $ do
          H.div ! A.class_ "flex justify-center mb4" $
            H.div ! A.class_ "h5 w-auto self-center" $
              H.div ! A.class_ "bg-navy light-green flex justify-between h5" $ do
                H.div ! A.class_ "flex flex-column justify-between pa4" $ do
                  H.h2 ! A.class_ "f3 f2-m f2-l fw8 lh-title mv1" $
                    "01. Auto Width"
                  H.div $ do
                    H.p ! A.class_ "f7 b tracked lh-copy ttu mv1 o-50" $
                      "Description"
                    H.p ! A.class_ "f5 lh-copy mv1" $
                      "This box takes up the size of its content and is centered."
                H.b ! A.class_ "flex items-center justify-start pa3 bl f6"
                    ! A.style "writing-mode:vertical-lr;text-orientation:mixed"
                    $ "Hypered Design System"
          H.div ! A.class_ "nl4 nr4 mb4" $
            H.div ! A.class_ "bg-navy light-green flex justify-between h5" $ do
              H.div ! A.class_ "flex flex-column justify-between pa4" $ do
                H.h2 ! A.class_ "f3 f2-m f2-l fw8 lh-title mv1" $
                  "02. Negative Margins"
                H.div $ do
                  H.p ! A.class_ "f7 b tracked lh-copy ttu mv1 o-50" $
                    "Description"
                  H.p ! A.class_ "f5 lh-copy mv1" $
                    "This box has negative margins and should extend beyond its parent's container."
              H.b ! A.class_ "flex items-center justify-start pa3 bl f6"
                  ! A.style "writing-mode:vertical-lr;text-orientation:mixed"
                  $ "Hypered Design System"
          H.div ! A.class_ "w-100" $
            H.div ! A.class_ "bg-navy light-green flex justify-between h5" $ do
              H.div ! A.class_ "flex flex-column justify-between pa4" $ do
                H.h2 ! A.class_ "f3 f2-m f2-l fw8 lh-title mv1" $
                  "03. Full Width"
                H.div $ do
                  H.p ! A.class_ "f7 b tracked lh-copy ttu mv1 o-50" $
                    "Description"
                  H.p ! A.class_ "f5 lh-copy mv1" $
                    "This box takes up the full width of its parent's container."
              H.b ! A.class_ "flex items-center justify-start pa3 bl f6"
                  ! A.style "writing-mode:vertical-lr;text-orientation:mixed"
                  $ "Hypered Design System"
    H.footer $ do
      H.hr ! A.class_ "bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4 bw1 b--black"
      H.p ! A.class_ "inline-flex lh-copy" $ "© Hypered, 2019-2023."


--------------------------------------------------------------------------------
-- TODO Move this to Refli.
homePageRefli :: Html
homePageRefli = do
  pageRefli "Bienvenue" $ do
    H.p $ do
      "Refli explore le calcul de la paie en Belgique."
    describeFormRefli
    H.p $
       H.a ! A.href "/fr/describe" $ "→ Voir des exemples de calculs."
    H.p H.br
  H.preEscapedToMarkup @Text
    "\n<!-- POKE k/98vRMnMyPsAY582jPTj4STOb2DWle0NO -->\n"

pageRefli :: Text -> Html -> Html
pageRefli title content = do
  H.div ! A.class_ "flex flex-column justify-between min-height-vh-100 mw8 center pa3 pa4-ns lh-copy" $ do
    H.div $ do
      H.header $
        navigationRefli
      H.main $
        H.article ! A.class_ "mw7" $ do
          H.div ! A.class_ "mb4" $
            H.h1 ! A.class_ "f1 lh-title mv2 tracked-tight" $ H.text title
          content
    navigationBlockRefli
    footerRefli

navigationRefli :: Html
navigationRefli =
  H.nav ! A.class_ "flex justify-between align-items-center lh-copy mb4 pv3" $
    H.div $
      H.a ! A.class_ "link mr3 black hover-blue" ! A.href "/fr" $ "Refli"

navigationBlockRefli :: Html
navigationBlockRefli = do
    H.hr ! A.class_ "bt bb-0 br-0 bl-0 mh0 mt4 pb1 w4 bw1 b--black"
    H.div ! A.class_ "mv5 flex-ns" $ do
      H.section ! A.class_ "w-60-ns pr4 mb5" $ do
        H.h1 ! A.class_ "f5 ttu lh-title mb3" $
          "Refli"
        H.ul ! A.class_ "list pa0 ma0 lh-copy" $ do
          H.li ! A.class_ "mr4" $
            H.a ! A.href "/fr/about" $ "A propos"
          H.li ! A.class_ "mr4" $
            H.a ! A.href "/changelog" $ "Changelog"
          H.li ! A.class_ "mr4" $
            H.a ! A.href "/fr/contact" $ "Contact"
          H.li ! A.class_ "mr4" $
            H.a ! A.href "/fr/disclaimer" $ "Disclaimer"
          H.li ! A.class_ "mr4" $
            H.a ! A.href "/fr/documentation" $ "Documentation"
      H.section ! A.class_ "w-50-ns pr4 mb5" $ do
        H.h1 ! A.class_ "f5 ttu lh-title mb3" $
          "Précompte professionnel"
        H.ul ! A.class_ "list pa0 ma0 lh-copy" $
          H.li ! A.class_ "mr4" $
            H.a ! A.href "/fr/documentation/computation#frais-professionnels-forfaitaires" $
              "Frais professionnels forfaitaires"
      H.section ! A.class_ "w-50-ns pr4 mb5" $ do
        H.h1 ! A.class_ "f5 ttu lh-title mb3" $
          "Contributions à la sécurité sociale"
        H.ul ! A.class_ "list pa0 ma0 lh-copy" $ do
          H.li ! A.class_ "mr4" $
            H.a ! A.href "/fr/documentation/computation#cotisations-personnelles" $
              "Cotisations personnelles"
          H.li ! A.class_ "mr4" $
            H.a ! A.href "/fr/documentation/computation#bonus-à-lemploi" $
              "Bonus à l'emploi"
          H.li ! A.class_ "mr4" $
            H.a ! A.href "/fr/documentation/computation#cotisation-spéciale" $
              "Cotisation spéciale"

footerRefli :: Html
footerRefli =
    H.footer $ do
      H.hr ! A.class_ "bt bb-0 br-0 bl-0 mh0 mt1 pb4 w4 bw1 b--black"
      H.p ! A.class_ "inline-flex lh-copy" $ "© Hypered SRL, 2023."

describeFormRefli :: Html
describeFormRefli = do
  formPost "/a/describe" $ do
    formBody "Calcul de salaire" $
      formFieldTextSmall "monthly-gross-salary" "Salaire mensuel brut" Nothing
    formButtons $ do
      formButton "Calculer →"


--------------------------------------------------------------------------------
-- | This is a script to connect to the backend using websocket, and reload the
-- page when the connection is lost (and then successfully re-created). You can
-- thus add this element temporarily to a page when you're hacking at it using
-- something like ghcid.
-- This can be used as an element given to the `document'` function.
autoReload :: Html
autoReload =
  H.preEscapedText
    "<script>\n\
  \function connect(isInitialConnection) {\n\
  \  // Create WebSocket connection.\n\
  \  var ws = new WebSocket('ws://' + location.host + '/ws');\n\
  \\n\
  \  // Connection opened\n\
  \  ws.onopen = function() {\n\
  \    ws.send('Hello server.');\n\
  \    if (isInitialConnection) {\n\
  \      console.log('autoreload: Initial connection.');\n\
  \    } else {\n\
  \      console.log('autoreload: Reconnected.');\n\
  \      location.reload();\n\
  \    };\n\
  \  };\n\
  \\n\
  \  // Listen for messages.\n\
  \  ws.onmessage = function(ev) {\n\
  \    console.log('autoreload: Message from server: ', ev.data);\n\
  \  };\n\
  \\n\
  \  // Trying to reconnect when the socket is closed.\n\
  \  ws.onclose = function(ev) {\n\
  \    console.log('autoreload: Socket closed. Trying to reconnect in 0.5 second.');\n\
  \    setTimeout(function() { connect(false); }, 500);\n\
  \  };\n\
  \\n\
  \  // Close the socker upon error.\n\
  \  ws.onerror = function(err) {\n\
  \    console.log('autoreload: Socket errored. Closing socket.');\n\
  \    ws.close();\n\
  \  };\n\
  \}\n\
  \\n\
  \connect(true);\n\
  \</script>\n"
