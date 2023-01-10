{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypered.Design.Run
  ( run
  ) where

import           Data.List (nub, tail)
import qualified Data.Text                     as T
import qualified Hypered.Design.Command        as Command
import           Hypered.Html
  ( footer, generateHtml, navigation, navigationTemplate, headTitle, partialHtml
  , prettyHtml, wrap , wrapPost , Config(..), Font(Inter, Font)
  )
import           Hypered.Stories (stories)
import           Protolude
import           Text.Blaze.Html5 ((!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)
import           Text.Blaze.Html.Renderer.Text (renderHtml)




import Hypered.Html
  ( Font(IbmPlex)
  , anchorBlue, anchorBlack
  , codeBlock, bannerGreen, bannerRed, bannerYellow
  , blockquote, pullQuote, pullQuote'
  , buttonFullWidth, buttonPrimary, buttonPrimaryDisabled, buttonSecondary
  , buttonSecondaryDisabled, defaultConfig, document
  , exampleLoginForm, exampleRegisterForm, exampleResetForm
  , exampleSidebar, exampleSidePanel
  , generate, generate', loginForm
  , nav, navigationNoteed, navigationNoteed'
  , tableDefault
  )


--------------------------------------------------------------------------------
run :: Command.Command -> IO ()
run (Command.GenerateTemplates forGitHubPages) = generateTemplates forGitHubPages

run Command.GenerateGuide = generateGuide

run Command.Wrapper = generateWrapper

run Command.GenerateAnchorBlue = generateAnchorBlue

run Command.GenerateAnchorBlack = generateAnchorBlack

run Command.GenerateNav = generateNav

run Command.GenerateBannerGreen = generateBannerGreen

run Command.GenerateBannerRed = generateBannerRed

run Command.GenerateBannerYellow = generateBannerYellow

run Command.GenerateBlockquoteDefault = generateBlockquoteDefault

run Command.GenerateBlockquotePullQuoteExample = generateBlockquotePullQuoteExample

run Command.GenerateBlockquoteWithOptionalPullQuoteExample =
  generateBlockquoteWithOptionalPullQuoteExample

run Command.GenerateButtonPrimary = generateButtonPrimary

run Command.GenerateButtonPrimaryDisabled = generateButtonPrimaryDisabled

run Command.GenerateButtonSecondary = generateButtonSecondary

run Command.GenerateButtonSecondaryDisabled = generateButtonSecondaryDisabled

run Command.GenerateButtonFullWidth = generateButtonFullWidth

run Command.GenerateFooter = generateFooter

run Command.GenerateTableDefault = generateTableDefault

run Command.GenerateFormLogin = generateFormLogin

run Command.GenerateNavigation = generateNavigation

run Command.GenerateNavigationSpaceBetween = generateNavigationSpaceBetween

run Command.GenerateLayoutDefault = generateLayoutDefault

run Command.ListCategories = listCategories

run Command.ListStories = listStories

run Command.JsImportStories = jsImportStories

run Command.JsStories = jsStories


--------------------------------------------------------------------------------
generateTemplates :: Bool -> IO ()
generateTemplates forGitHubPages = do
  let config = if forGitHubPages
               then
                 -- Used to generate HTML for GitHub Pages
                 Config "/static" Inter False
               else
                 -- Used to generate the template.
                 Config "$prefix$/static" (Font "$font$") False

  -- TODO The $body$ is indented when using the pretty printer, which
  -- then causes Pandoc to indent part of <code> content, which
  -- is wrong. Same for lots of white space betwee navigation links, which
  -- causes also some additional "padding".
  generateHtml config "generated/templates" "default.html" "$title$"
    (H.div (navigationTemplate >> wrapPost "$title$" "$body$") >> footer "$footer$")

  -- TODO Currently reusing the default.html template.
  prettyHtml config "generated/templates" "default-2-cols.html" "$title$"
    (wrap "$body$" >> footer "$footer$")

  -- TODO Currently reusing the default.html template.
  prettyHtml config "generated/templates" "poster.html" "$title$"
    (wrap "$body$" >> footer "$footer$")

  -- We probably don't need the footer, navigation, and title partial
  -- templates since they can be generated with the complete templates.

  partialHtml config "generated/templates" "footer.html" "" (footer "$footer$")
  partialHtml config "generated/templates" "navigation.html" "" (navigation ".")
  partialHtml config "generated/templates" "title.html" "" headTitle


------------------------------------------------------------------------------
generateGuide :: IO ()
generateGuide = do
  let conf = defaultConfig { cAddWrapper = False }

  generate' "index.html" "Hypered style guide" conf $ \_ -> do
    H.h1 "Components and examples"
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

      H.li $ H.a ! A.href "form--login.html" $ "Form, login"

      H.li $ H.a ! A.href "example--login-form.html" $ "Example, login form"
      H.li $ H.a ! A.href "example--sidebar.html" $ "Example, sidebar"
      H.li $ H.a ! A.href "example--side-panel.html" $ "Example, side panel"

      H.li $ H.a ! A.href "example--template.html" $ "Example, template"

      H.li $ H.a ! A.href "example--login-form-ibm-plex.html" $
        "Example, login form (IBM Plex)"
      H.li $ H.a ! A.href "example--register-form-ibm-plex.html" $
        "Example, register form (IBM Plex)"
      H.li $ H.a ! A.href "example--reset-form-ibm-plex.html" $
        "Example, reset form (IBM Plex)"
      H.li $ H.a ! A.href "example--sidebar-ibm-plex.html" $
        "Example, sidebar (IBM Plex)"
      H.li $ H.a ! A.href "example--side-panel-ibm-plex.html" $
        "Example, side panel (IBM Plex)"

      H.li $ H.a ! A.href "example--template-ibm-plex.html" $
        "Example, template (IBM Plex)"

  -- Horizontal navigation bar:
  -- This is mostly header / nav / a, a, ...

  generate "navigation.html" "Hypered style guide - Navigation" navigation

  -- Banner

  generate "banner--green.html" "Hypered style guide - Banner"
    (const bannerGreenExample)
  generate "banner--red.html" "Hypered style guide - Banner"
    (const bannerRedExample)
  generate "banner--yellow.html" "Hypered style guide - Banner"
    (const bannerYellowExample)

  -- Button

  generate "button--primary.html" "Hypered style guide - Button"
    (const buttonPrimaryExample)
  generate "button--primary-disabled.html" "Hypered style guide - Button"
    (const buttonPrimaryDisabledExample)
  generate "button--secondary.html" "Hypered style guide - Button"
    (const buttonSecondaryExample)
  generate "button--secondary-disabled.html" "Hypered style guide - Button"
    (const buttonSecondaryDisabledExample)
  generate "button--full-width.html" "Hypered style guide - Button"
    (const buttonFullWidthExample)

  -- Code block

  generate "code-block.html" "Hypered style guide - Code block"
    (const codeBlock)

  -- Forms

  generate "form--login.html" "Hypered style guide - Form"
    (const loginForm)

  -- Footer

  generate "footer.html" "Hypered style guide - Footer"
    (const footerExample)


  -- Example usage

  generate' "example--login-form.html" "Hypered style guide - Login Form Example"
    conf (const exampleLoginForm)
  generate' "example--sidebar.html" "Hypered style guide - Sidebar Example"
    conf (const exampleSidebar)
  generate' "example--side-panel.html" "Hypered style guide - Side panel Example"
    conf (const exampleSidePanel)

  -- Example usage using IBM Plex

  let conf' = conf { cFont = IbmPlex }
  generate' "example--login-form-ibm-plex.html" "Hypered style guide - Login Form Example"
    conf' (const exampleLoginForm)
  generate' "example--register-form-ibm-plex.html"
    "Hypered style guide - Register Form Example"
    conf' (const exampleRegisterForm)
  generate' "example--reset-form-ibm-plex.html"
    "Hypered style guide - Reset Form Example"
    conf' (const exampleResetForm)
  generate' "example--sidebar-ibm-plex.html" "Hypered style guide - Sidebar Example"
    conf' (const exampleSidebar)
  generate' "example--side-panel-ibm-plex.html" "Hypered style guide - Side panel Example"
    conf' (const exampleSidePanel)


------------------------------------------------------------------------------
-- The document wrapper. This should match `pages/_app.js`.
generateWrapper :: IO ()
generateWrapper = do
  putStr (Pretty.renderHtml
    ( document defaultConfig "wrapper.html" "Hypered design system"
      ( H.preEscapedToHtml @Text "<!-- CONTENT MARKER -->"
    )))


------------------------------------------------------------------------------
-- Individual components

generateAnchorBlue :: IO ()
generateAnchorBlue = putStr $ renderHtml anchorBlueExample

generateAnchorBlack :: IO ()
generateAnchorBlack = putStr $ renderHtml anchorBlackExample

generateNav :: IO ()
generateNav = putStr $ renderHtml (nav "")

generateBannerGreen :: IO ()
generateBannerGreen = putStr $ renderHtml bannerGreenExample

generateBannerRed :: IO ()
generateBannerRed = putStr $ renderHtml bannerRedExample

generateBannerYellow :: IO ()
generateBannerYellow = putStr $ renderHtml bannerYellowExample

generateBlockquoteDefault :: IO ()
generateBlockquoteDefault = putStr $ renderHtml blockquoteDefault

generateBlockquotePullQuoteExample :: IO ()
generateBlockquotePullQuoteExample = putStr $ renderHtml blockquotePullQuoteExample

generateBlockquoteWithOptionalPullQuoteExample :: IO ()
generateBlockquoteWithOptionalPullQuoteExample =
  putStr $ renderHtml blockquoteWithOptionalPullQuoteExample

generateButtonPrimary :: IO ()
generateButtonPrimary = putStr $ renderHtml buttonPrimaryExample

generateButtonPrimaryDisabled :: IO ()
generateButtonPrimaryDisabled = putStr $ renderHtml buttonPrimaryDisabledExample

generateButtonSecondary :: IO ()
generateButtonSecondary = putStr $ renderHtml buttonSecondaryExample

generateButtonSecondaryDisabled :: IO ()
generateButtonSecondaryDisabled = putStr $ renderHtml buttonSecondaryDisabledExample

generateButtonFullWidth :: IO ()
generateButtonFullWidth = putStr $ renderHtml buttonFullWidthExample

generateFooter :: IO ()
generateFooter = putStr $ renderHtml footerExample

generateTableDefault :: IO ()
generateTableDefault = putStr $ renderHtml tableDefaultExample


------------------------------------------------------------------------------
anchorBlueExample :: Html
anchorBlueExample = anchorBlue "#" "This is a blue link"

anchorBlackExample :: Html
anchorBlackExample = anchorBlack "#" "This is a black link"

bannerGreenExample :: Html
bannerGreenExample = bannerGreen "Messages sent!"

bannerRedExample :: Html
bannerRedExample = bannerRed "Error, something is wrong."

bannerYellowExample :: Html
bannerYellowExample = bannerYellow "Something might be wrong."

blockquoteDefault :: Html
blockquoteDefault = blockquote $
  "You have power over your mind - not outside events. \
  \Realize this, and you will find strength."

blockquotePullQuoteExample :: Html
blockquotePullQuoteExample = pullQuote'
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. \
  \Nullam consectetur tincidunt elit, et semper enim laoreet eu. \
  \In hac habitasse platea dictumst. \
  \Phasellus consequat quis augue vitae laoreet. \
  \In consequat, urna vel volutpat dignissim, eros eros sodales quam, \
  \a suscipit felis eros non dolor."

blockquoteWithOptionalPullQuoteExample :: Html
blockquoteWithOptionalPullQuoteExample = pullQuote
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. \
  \Nullam consectetur tincidunt elit, et semper enim laoreet eu. \
  \In hac habitasse platea dictumst. \
  \Phasellus consequat quis augue vitae laoreet. \
  \In consequat, urna vel volutpat dignissim, eros eros sodales quam, \
  \a suscipit felis eros non dolor."

buttonPrimaryExample :: Html
buttonPrimaryExample = buttonPrimary "Primary Button"

buttonPrimaryDisabledExample :: Html
buttonPrimaryDisabledExample = buttonPrimaryDisabled "Primary Button"

buttonSecondaryExample :: Html
buttonSecondaryExample = buttonSecondary "Secondary Button"

buttonSecondaryDisabledExample :: Html
buttonSecondaryDisabledExample = buttonSecondaryDisabled "Secondary Button"

buttonFullWidthExample :: Html
buttonFullWidthExample = buttonFullWidth "Primary Button"

footerExample :: Html
footerExample = footer "© Hypered, 2019-2023."

tableDefaultExample :: Html
tableDefaultExample = tableDefault


------------------------------------------------------------------------------
-- Stories from Storybook

generateFormLogin :: IO ()
generateFormLogin = putStr (renderHtml loginForm)

generateNavigation :: IO ()
generateNavigation = putStr (renderHtml (navigationNoteed))

generateNavigationSpaceBetween :: IO ()
generateNavigationSpaceBetween = putStr (renderHtml (navigationNoteed'))

generateLayoutDefault :: IO ()
generateLayoutDefault = putStr (renderHtml (nav ""))


------------------------------------------------------------------------------
-- Helpers to explore Storybook stories

listCategories :: IO ()
listCategories = mapM_ putStrLn (nub ((map fst (tail stories))))

listStories :: IO ()
listStories = mapM_ putStrLn (map dashdash (tail stories))

jsImportStories :: IO ()
jsImportStories = mapM_ (putStrLn . jsimport) (nub (map fst (tail stories)))

jsStories :: IO ()
jsStories = mapM_ putStrLn (map js (tail stories))


------------------------------------------------------------------------------
dashdash :: (Text, Text) -> Text
dashdash (a, b) = case T.uncons b of
  Nothing -> T.toLower a
  Just (hd, tl) -> T.toLower (a <> "--" <> T.singleton hd <> T.concatMap f tl)
  where
  f c | isUpper c = T.pack ['-', c]
  f c | isDigit c = T.pack ['-', c]
      | otherwise = T.singleton c

jsimport :: Text -> Text
jsimport a =
  "var " <> a <> " = require(\"./components/" <> a <> "/" <> a <> ".stories\");"

js :: (Text, Text) -> Text
js (a, b) = unlines
  [ "case '" <> dashdash (a, b) <> "':"
  , "  render(" <> a <> "." <> b <> "());"
  , "  break;"
  ]
