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

buttonPrimaryDisabled :: Html -> Html
buttonPrimaryDisabled = H.button
  ! A.class_ "button-reset ph4 pv3 bg-black white ba bw1 b--black o-50"
  ! A.disabled ""

buttonSecondary :: Html -> Html
buttonSecondary = H.button
  ! A.class_ "button-reset ph4 pv3 bg-white black ba bw1 b--black"

buttonSecondaryDisabled :: Html -> Html
buttonSecondaryDisabled = H.button
  ! A.class_ "button-reset ph4 pv3 bg-white black ba bw1 b--black o-50"
  ! A.disabled ""

buttonFullWidth :: Html -> Html
buttonFullWidth = H.button
  ! A.class_ "button-reset ph4 pv3 bg-black white ba bw1 b--black w-100"

-- TODO When pretty-printing the HTML, the first line within the code element
-- is indented, which is not correct.
codeBlock :: Html
codeBlock = H.pre ! A.class_ "pre overflow-auto" $ H.code $
  "// this is a comment\n\
  \// this is another comment\n\
  \// this is a slightly longer comment\n"

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
  H.a ! A.class_ "link black hover-blue" ! A.href href $ content

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
exampleSidebar = do
  H.header $
    navigationNoteed
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
          "not-os is a minimal OS based on the Linux kernel, coreutils,"
          "runit, and Nix. It is also the build script, written in Nix"
          "expressions, to build such OS."
        H.p ! A.class_ "f5 lh-copy mv3" $ do
          "This is a project of Michael Bishop (cleverca22 on GitHub, clever on"
          "IRC). I modified it just a bit to make it possible to generate this"
          "documentation."
        H.p ! A.class_ "f5 lh-copy mv3" $ do
          "As a build tool, not-os uses nixpkgs and in particular the"
          H.a ! A.href "https://nixos.wiki/wiki/NixOS_Modules" $ "NixOS module system"
          "to build the three main components of a Linux-based operating"
          "system:"
  footer "© Võ Minh Thu, 2017-2023."

exampleSidePanel :: Html
exampleSidePanel = do
  H.header $
    navigationNoteed
  H.main $ do
    H.div ! A.class_ "flex flex-wrap nl3 nr3" $ do
      H.main ! A.class_ "w-100 w-80-m w-80-l ph3" $
        H.article $ do
          H.h1 ! A.class_ "f1 lh-title mv2 tracked-tight" $ "Waveguide"
          H.p ! A.class_ "f5 lh-copy mv3" $ do
            "If neither a list of attribute names or a command are given,"
            "Waveguide instrospects the Nix expression and builds all the"
            "found attributes."
      H.aside ! A.class_ "w-100 w-20-m w-20-l ph3 mt0 mt5-m mt5-l" $
        H.div ! A.class_ "nl3 nr3" $ do
          H.h3 ! A.class_ "f5 lh-title mv2" $ "Latest Runs"
          H.ul ! A.class_ "bg-near-white list pa3" $ do
            H.li ! A.class_ "pv1 bb b--black-10" $
              H.a ! A.class_ "link no-underline black blue-hover" $ "→ #001"
            H.li ! A.class_ "pv1 bb b--black-10" $
              H.a ! A.class_ "link no-underline black blue-hover" $ "→ #002"
            H.li ! A.class_ "pv1 bb b--black-10" $
              H.a ! A.class_ "link no-underline black blue-hover" $ "→ #003"
            H.li ! A.class_ "pv1 bb b--black-10" $
              H.a ! A.class_ "link no-underline black blue-hover" $ "→ #004"
            H.li ! A.class_ "pv1 bb b--black-10" $
              H.a ! A.class_ "link no-underline black blue-hover" $ "→ #005"
  footer "© Võ Minh Thu, 2017-2023."


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
tableDefault :: Html
tableDefault =
  H.div ! A.class_ "overflow-x-scroll" $
    H.table ! A.class_ "bg-white collapse w-100" $ do
      H.thead $
        H.tr ! A.class_ "b--black bw1 bb b--black" $ do
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl" $ "Column 1"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl" $ "Column 2"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl" $ "Column 3"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl" $ "Column 4"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tr"
               ! customAttribute "align" "right" $
            "Column 5"
      H.tbody $
        replicateM_ 10 $
          H.tr ! A.class_ "b--black bb b--black" $ do
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
        H.tr ! A.class_ "b--black bw1 bb b--black" $ do
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl" $ "Column 1"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl" $ "Column 2"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl" $ "Column 3"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl" $ "Column 4"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tr"
               ! customAttribute "align" "right" $
            "Column 5"
      H.tbody $
        replicateM_ 10 $
          H.tr ! A.class_ "b--black bb b--black" $ do
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
        H.tr ! A.class_ "b--black bw1 bb b--black" $ do
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl br" $ "Column 1"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl br" $ "Column 2"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl br" $ "Column 3"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tl br" $ "Column 4"
          H.th ! A.class_ "tl fw6 nowrap f5 pa2 tr"
               ! customAttribute "align" "right" $
            "Column 5"
      H.tbody $ do
        replicateM_ 9 $
          H.tr ! A.class_ "b--black bb b--black" $ do
            H.td ! A.class_ "nowrap f5 pa2 tl br" $ "Red"
            H.td ! A.class_ "nowrap f5 pa2 tl br" $ "Green"
            H.td ! A.class_ "nowrap f5 pa2 tl br" $ "Blue"
            H.td ! A.class_ "nowrap f5 pa2 tl br" $ "Yellow"
            H.td ! A.class_ "nowrap f5 pa2 tr"
                 ! customAttribute "align" "right" $ "001"
        replicateM_ 1 $
          H.tr ! A.class_ "b--black b--black" $ do
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
        H.tr ! A.class_ "b--black bw1 bb b--black" $ do
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl br" $ "Column 1"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl br" $ "Column 2"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl br" $ "Column 3"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tl br" $ "Column 4"
          H.th ! A.class_ "tl fw6 nowrap f6 pa1 tr"
               ! customAttribute "align" "right" $
            "Column 5"
      H.tbody $ do
        replicateM_ 9 $
          H.tr ! A.class_ "b--black bb b--black" $ do
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Red"
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Green"
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Blue"
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Yellow"
            H.td ! A.class_ "nowrap f6 pa1 tr"
                 ! customAttribute "align" "right" $ "001"
        replicateM_ 1 $
          H.tr ! A.class_ "b--black b--black" $ do
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Red"
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Green"
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Blue"
            H.td ! A.class_ "nowrap f6 pa1 tl br" $ "Yellow"
            H.td ! A.class_ "nowrap f6 pa1 tr"
                 ! customAttribute "align" "right" $ "001"
