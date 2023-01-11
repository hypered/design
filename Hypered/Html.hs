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
    T.hPutStr h . renderHtml $ document config path title body

prettyHtml :: Config -> FilePath -> FilePath -> Text -> Html -> IO ()
prettyHtml config base path title body = do
  createDirectoryIfMissing True (takeDirectory (base </> path))
  withFile (base </> path) WriteMode $ \h ->
    hPutStr h . Pretty.renderHtml $ document config path title body'
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
  , cFont = Inter
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
        mapM_ (\a -> H.toHtml ("@import url(" <> relativize a <> ");"))
          [ cStaticPath </> fontCss cFont
          , cStaticPath </> "css/tachyons.min.v4.11.1.css"
          , cStaticPath </> "css/style.css"
          , cStaticPath </> "css/styles.css"
          ]

    H.body ! A.class_ (H.toValue (fontClass cFont)) $
      H.div ! A.class_ "flex flex-column justify-between min-height-vh-100 mw8 center pa3 pa4-ns lh-copy" $
        body


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
wrapPost :: Html -> Html -> Html
wrapPost title content =
  H.main $
    H.article ! A.class_ "mw7" $ do
      H.div ! A.class_ "mb4" $ do
        H.h1 ! A.class_ "f1 lh-title mv2 tracked-tight" $
          title
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
-- within the code element is indented, which is not correct. Instead use
-- H.preEscapedToHtml to force what we want.
codeBlock :: Text -> Html
codeBlock content = H.pre ! A.class_ "pre overflow-auto" $ do
  H.preEscapedToHtml @Text "<code class=\"code\">"
  H.text content
  H.preEscapedToHtml @Text "</code>"


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
    H.div ! A.class_ "flex flex-wrap nl2 nr2 mw8" $
      H.div ! A.class_ "w-100 w-50-l ph2 mb3" $ do
        H.div ! A.class_ "hy-bg-blue light-green aspect-ratio aspect-ratio--4x3 relative" $
          card
        H.div ! A.class_ "hy-bg-red light-yellow aspect-ratio aspect-ratio--4x3 relative" $
          card
        H.div ! A.class_ "hy-bg-green black aspect-ratio aspect-ratio--4x3 relative" $
          card
        H.div ! A.class_ "hy-bg-yellow hy-red aspect-ratio aspect-ratio--4x3 relative" $
          card
        H.div ! A.class_ "hy-bg-blue white aspect-ratio aspect-ratio--4x3 relative" $
          card
        H.div ! A.class_ "hy-bg-red white aspect-ratio aspect-ratio--4x3 relative" $
          card
        H.div ! A.class_ "hy-bg-green white aspect-ratio aspect-ratio--4x3 relative" $
          card
        H.div ! A.class_ "hy-bg-yellow white aspect-ratio aspect-ratio--4x3 relative" $
          card
 where
  card =
    H.div ! A.class_ "aspect-ratio--object flex items-stretch" $ do
      H.div ! A.class_ "flex flex-column justify-between pa4 h-100" $ do
        H.h2 ! A.class_ "f2 f1-m f1-l mv2 fw6 tracked-tight lh-title" $ do
          "Samples"
          H.span ! A.class_ "o-50" $ "↗"
        H.p ! A.class_ "f6 f5-m f5-l lh-copy" $
          "The user interface (UI), in the industrial design field of human-computer interaction, is the space where interactions between humans and machines occur."
      H.div ! A.class_ "bl w4" $
        H.div ! A.class_ "flex justify-between items-center pa3 h-100"
              ! A.style "writing-mode:vertical-lr;text-orientation:mixed" $ do
          H.div ! A.class_ "b" $ "Hypered Design System"
          H.div $ "Volume 001"


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

sidebarLink :: Html -> H.AttributeValue -> Html
sidebarLink content href =
  H.a ! A.class_ "black no-underline hy-hover-blue" ! A.href href $ content

sidebar :: [(Html, [(Html, H.AttributeValue)])] -> Html
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
      sidebarLink name href

exampleLoginForm :: Html
exampleLoginForm = do
  H.header $
    navigationReesd
  H.p "Reesd is in private alpha. New registrations are currently disabled."
  loginForm
  -- There could be a footer, but on simple forms, I think I prefer without.
  -- footer "© Hypered, 2020-2023."

exampleRegisterForm :: Html
exampleRegisterForm = do
  H.header $
    navigationReesd
  H.p "Reesd is in private alpha. New registrations are currently disabled."
  registerForm
  -- There could be a footer, but on simple forms, I think I prefer without.
  -- footer "© Hypered, 2020-2023."

exampleResetForm :: Html
exampleResetForm = do
  H.header $
    navigationReesd
  H.p "Enter a verified email address and we'll send a password reset link\
    \ to that address."
  resetForm
  -- There could be a footer, but on simple forms, I think I prefer without.
  -- footer "© Hypered, 2020-2023."

exampleSidebar :: Html
exampleSidebar =
  H.div ! A.class_ "flex flex-column justify-between hy-min-height-vh-100 mw8 center pa4 lh-copy" $ do
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
            H.h1 ! A.class_ "f1 lh-title mv2 tracked-tight" $ "not-os"
            H.p ! A.class_ "f5 lh-copy mv3" $ do
              "not-os is a minimal OS based on the Linux kernel, coreutils, "
              "runit, and Nix. It is also the build script, written in Nix "
              "expressions, to build such OS."
            H.p ! A.class_ "f5 lh-copy mv3" $ do
              "This is a project of Michael Bishop (cleverca22 on GitHub, clever on "
              "IRC). I modified it just a bit to make it possible to generate this "
              "documentation."
            H.p ! A.class_ "f5 lh-copy mv3" $ do
              "As a build tool, not-os uses nixpkgs and in particular the "
              H.a ! A.href "https://nixos.wiki/wiki/NixOS_Modules" $ "NixOS module system"
              " to build the three main components of a Linux-based operating "
              "system:"
    footer "© Hypered, 2019-2023."

exampleSidePanel :: Html
exampleSidePanel = do
  H.div ! A.class_ "flex flex-column justify-between hy-min-height-vh-100 mw8 center pa4 lh-copy" $ do
    H.div $ do
      H.header $
        navigationNoteedX
      -- H.main $
      identity $ -- TODO I think H.main should be present.
        H.div ! A.class_ "flex flex-wrap nl3 nr3" $ do
          H.main ! A.class_ "w-100 w-80-m w-80-l ph3" $
            H.article $ do
              H.h1 ! A.class_ "f1 lh-title mv2 tracked-tight" $ "Waveguide"
              H.p ! A.class_ "f5 lh-copy mv3" $ do
                "If neither a list of attribute names or a command are given, "
                "Waveguide instrospects the Nix expression and builds all the "
                "found attributes."
          H.aside ! A.class_ "order-1 order-2-m order-2-l w-100 w-20-m w-20-l ph3 mt2" $
            H.div ! A.class_ "" $ do
              H.h3 ! A.class_ "f5 lh-title mv2" $ "Latest Runs"
              H.ul ! A.class_ "bg-near-white list pa3" $ do
                H.li ! A.class_ "pv1 bb b--black-10" $
                  H.a ! A.class_ "black no-underline hy-hover-blue" $ "→ #001"
                H.li ! A.class_ "pv1 bb b--black-10" $
                  H.a ! A.class_ "black no-underline hy-hover-blue" $ "→ #002"
                H.li ! A.class_ "pv1 bb b--black-10" $
                  H.a ! A.class_ "black no-underline hy-hover-blue" $ "→ #003"
                H.li ! A.class_ "pv1 bb b--black-10" $
                  H.a ! A.class_ "black no-underline hy-hover-blue" $ "→ #004"
                H.li ! A.class_ "pv1 bb b--black-10" $
                  H.a ! A.class_ "black no-underline hy-hover-blue" $ "→ #005"
    footer "© Hypered, 2019-2023."


------------------------------------------------------------------------------
-- Forms
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Login form
-- https://hypered.github.io/design/storybook/?path=/story/form--login
loginForm :: Html
loginForm = do
  H.form ! A.class_ "bg-white mw6"
         ! A.method "POST"
         ! A.action "/a/login"
         $ do
    H.div ! A.class_ "pa4 bt br bl b--black bw1" $ do
      H.h2 "Log in to Reesd"
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
      H.a ! A.class_ "black no-underline hy-hover-blue"
          ! A.href "/reset"
          $ "Reset password"
    H.div ! A.class_ "flex justify-between" $ do
      H.a ! A.class_ "bg-white b--black black ph3 pb4 pt3 tl w-100 dib no-underline ba bw1"
          ! A.href "/register"
          $ "Register"
      H.button ! A.class_ "bg-black b--black white ph3 pb4 pt3 tl w-100 button-reset ba bw1" $ "Log in —>"

-- | Register form
registerForm :: Html
registerForm = do
  H.form ! A.class_ "bg-white mw6"
         ! A.method "POST"
         ! A.action "/a/register"
         $ do
    H.div ! A.class_ "pa4 bt br bl b--black bw1" $ do
      H.h2 "Register for Reesd"
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
    H.div ! A.class_ "flex justify-between" $ do
      H.a ! A.class_ "bg-white b--black black ph3 pb4 pt3 tl w-100 dib no-underline ba bw1"
          ! A.href "/login"
          $ "Log in"
      H.button ! A.class_ "bg-black b--black white ph3 pb4 pt3 tl w-100 button-reset ba bw1" $ "Register —>"

-- | Reset form
resetForm :: Html
resetForm = do
  H.form ! A.class_ "bg-white mw6"
         ! A.method "POST"
         ! A.action "/a/reset"
         $ do
    H.div ! A.class_ "pa4 bt br bl b--black bw1" $ do
      H.h2 "Reset password for Reesd"
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
    H.div ! A.class_ "flex justify-between" $ do
      H.a ! A.class_ "bg-white b--black black ph3 pb4 pt3 tl w-100 dib no-underline ba bw1"
          ! A.href "/login"
          $ "Log in"
      H.button ! A.class_ "bg-black b--black white ph3 pb4 pt3 tl w-100 button-reset ba bw1" $ "Reset password —>"


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
            H.a ! A.class_ "black no-underline hy-hover-blue" $ "Item One"
          H.li $
            H.a ! A.class_ "black no-underline hy-hover-blue" $ "Item Two"
          H.li $
            H.a ! A.class_ "black no-underline hy-hover-blue" $ "Item Three"
 where
  links = do
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" $ "Item One"
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" $ "Item Two"
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" $ "Item Three"
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" $ "Item Four"
    H.li $
      H.a ! A.class_ "black no-underline hy-hover-blue" $ "Item Five"

navigationBlockUsageExample :: Html
navigationBlockUsageExample = navigationBlockDefault


------------------------------------------------------------------------------
navigationDefault :: Html
navigationDefault = navigationNoteed

navigationSpaceBetween :: Html
navigationSpaceBetween =
  H.nav ! A.class_ "flex justify-between align-items-center lh-copy mb4 pv3" $ do
    H.div $
      H.a ! A.class_ "black hy-hover-blue underline mr3" ! A.href "#" $
        "noteed.com"
    H.div $ do
      H.a ! A.class_ "black hy-hover-blue mr3" ! A.href "#" $ "blog"
      H.a ! A.class_ "black hy-hover-blue" ! A.href "#" $ "not-os"


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
        H.li ! A.class_ "pv1 bb b--black-10" $
          H.a ! A.class_ "black no-underline hy-hover-blue" $ "→ #001"
        H.li ! A.class_ "pv1 bb b--black-10" $
          H.a ! A.class_ "black no-underline hy-hover-blue" $ "→ #002"
        H.li ! A.class_ "pv1 bb b--black-10" $
          H.a ! A.class_ "black no-underline hy-hover-blue" $ "→ #003"
        H.li ! A.class_ "pv1 bb b--black-10" $
          H.a ! A.class_ "black no-underline hy-hover-blue" $ "→ #004"
        H.li ! A.class_ "pv1 bb b--black-10" $
          H.a ! A.class_ "black no-underline hy-hover-blue" $ "→ #005"

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
              H.p ! A.class_ "f5 lh-copy mv3" $
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
              H.p ! A.class_ "f5 lh-copy mv3" $
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
    H.span ! A.class_ "normal" $ "Design System"

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
      H.p ! A.class_ "f5 lh-copy mv3" $
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent convallis mollis nulla, molestie tempor velit consequat non."
    H.hr ! A.class_ "mt3 pb3 bt-0 bl-0 br-0 bb b--black"
    H.section ! A.class_ "pv4 pv5-l" $ do
      H.h2 ! A.class_ "f1 tracked-tight mv2" $ "Introduction"
      H.p ! A.class_ "f5 lh-copy mv3" $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent convallis mollis nulla, molestie tempor velit consequat non. Integer quam ligula, consequat eget semper in, sodales nec mauris. Sed ultrices enim quis eros lobortis, semper condimentum eros sodales. Morbi iaculis lectus id dui convallis feugiat."
    H.hr ! A.class_ "mt3 pb3 bt-0 bl-0 br-0 bb b--black"
    H.section ! A.class_ "pv4 pv5-l" $
      H.div ! A.class_ "flex flex-wrap nl3 nr3 tc" $ do
        H.div ! A.class_ "w-100 w-third-l ph3" $ do
          H.h2 ! A.class_ "f1 tracked-tight mv2" $ "23,000"
          H.p ! A.class_ "f5 lh-copy mv3" $ "downloads"
        H.div ! A.class_ "w-100 w-third-l ph3" $ do
          H.h2 ! A.class_ "f1 tracked-tight mv2" $ "3.2kb"
          H.p ! A.class_ "f5 lh-copy mv3" $ "gzipped"
        H.div ! A.class_ "w-100 w-third-l ph3" $ do
          H.h2 ! A.class_ "f1 tracked-tight mv2" $ "626"
          H.p ! A.class_ "f5 lh-copy mv3" $ "stars on GitHub"

titleDefaultUsageExample :: Html
titleDefaultUsageExample =
  H.div ! A.class_ "mw8 center" $ do
    H.header ! A.class_ "pv4 pv5-l" $ do
      H.h1 ! A.class_ "f1 f1-l tracked-tight mv2" $ "Hypered"
      H.h2 ! A.class_ "f2 tracked-tight mv2" $ "Software development, defined"
      H.p ! A.class_ "f5 lh-copy mv3" $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent convallis mollis nulla, molestie tempor velit consequat non."
    H.hr ! A.class_ "mt3 pb3 bt-0 bl-0 br-0 bb b--black"
    H.section ! A.class_ "pv4 pv5-l" $ do
      H.h2 ! A.class_ "f2 tracked-tight mv2" $ "Introduction"
      H.p ! A.class_ "f5 lh-copy mv3" $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent convallis mollis nulla, molestie tempor velit consequat non. Integer quam ligula, consequat eget semper in, sodales nec mauris. Sed ultrices enim quis eros lobortis, semper condimentum eros sodales. Morbi iaculis lectus id dui convallis feugiat."
    H.hr ! A.class_ "mt3 pb3 bt-0 bl-0 br-0 bb b--black"
    H.section ! A.class_ "pv4 pv5-l" $
      H.div ! A.class_ "flex flex-wrap nl3 nr3 tc" $ do
        H.div ! A.class_ "w-100 w-third-l ph3" $ do
          H.h2 ! A.class_ "f2 tracked-tight mv2" $ "23,000"
          H.p ! A.class_ "f5 lh-copy mv3" $ "downloads"
        H.div ! A.class_ "w-100 w-third-l ph3" $ do
          H.h2 ! A.class_ "f2 tracked-tight mv2" $ "3.2kb"
          H.p ! A.class_ "f5 lh-copy mv3" $ "gzipped"
        H.div ! A.class_ "w-100 w-third-l ph3" $ do
          H.h2 ! A.class_ "f2 tracked-tight mv2" $ "626"
          H.p ! A.class_ "f5 lh-copy mv3" $ "stars on GitHub"


--------------------------------------------------------------------------------
typographyHeading1Example :: Html
typographyHeading1Example =
  H.h1 ! A.class_ "f1 lh-title mv2 tracked-tight" $ "Heading 1"

typographyHeading2Example :: Html
typographyHeading2Example =
  H.h2 ! A.class_ "f2 lh-title mv2 tracked-tight" $ "Heading 2"

typographyHeading3Example :: Html
typographyHeading3Example =
  H.h3 ! A.class_ "f3 lh-title mv2 tracked-tight" $ "Heading 3"

typographyHeading4Example :: Html
typographyHeading4Example =
  H.h4 ! A.class_ "f4 lh-title mv2 tracked-tight" $ "Heading 4"

typographyHeading5Example :: Html
typographyHeading5Example =
  H.h5 ! A.class_ "f5 lh-title mv2" $ "Heading 5"

typographyHeading6Example :: Html
typographyHeading6Example =
  H.h6 ! A.class_ "f6 lh-title mv2 ttu" $ "Heading 6"

typographyParagraphExample :: Html
typographyParagraphExample =
  H.p ! A.class_ "f5 lh-copy mv3" $
   "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla sollicitudin malesuada est. Sed efficitur laoreet massa, eu dictum est luctus sit amet. Morbi elementum dapibus pellentesque. Sed varius nisi nisi, nec imperdiet nunc bibendum at. Maecenas bibendum, neque nec vehicula dignissim, sem dolor congue risus, ac consequat sapien nibh in felis. Suspendisse at est a nisl dictum condimentum. Suspendisse ut dolor vitae nisi dictum hendrerit a vel magna. Etiam porttitor lacus magna, non bibendum tellus lobortis sit amet. Duis quis lectus massa. Nam quis fringilla dui. Fusce felis leo, iaculis id dui eu, lacinia varius dolor. Praesent molestie rhoncus mi, ac malesuada neque placerat vitae. Aliquam placerat auctor pretium. Mauris egestas condimentum erat sit amet tempor. Quisque imperdiet, augue nec eleifend placerat, lacus tortor pellentesque metus, sit amet laoreet lectus enim et mauris. Vivamus sollicitudin a ex sit amet ullamcorper."

typographyUsageExample :: Html
typographyUsageExample =
  H.article ! A.class_ "measure-wide" $ do
    H.h1 ! A.class_ "f1 lh-title mv2 tracked-tight" $
      "Design System Blog"
    H.p ! A.class_ "f5 lh-copy mv3" $
      "This is an intro to using Hypered design system."
    H.h2 ! A.class_ "f2 lh-title mv2 tracked-tight" $ "Components"
    H.p ! A.class_ "f5 lh-copy mv3" $
      "Hypered design system comprises of components that quickly help you get started with your projects."
    H.p ! A.class_ "f5 lh-copy mv3" $ do
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
  H.div ! A.class_ "flex flex-column justify-between hy-min-height-vh-100 mw8 center pa4 lh-copy" $ do
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
