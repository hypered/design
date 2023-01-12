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
  , bannerGreen, bannerRed, bannerYellow
  , blockquote, pullQuote, pullQuote'
  , buttonPrimary, buttonPrimaryLarge, buttonPrimaryDisabled
  , buttonSecondary, buttonSecondaryLarge, buttonSecondaryDisabled
  , buttonFullWidth
  , buttonLinkPrimary, buttonLinkPrimaryLarge, buttonLinkPrimaryDisabled
  , buttonLinkSecondary, buttonLinkSecondaryLarge, buttonLinkSecondaryDisabled
  , buttonLinkFullWidth
  , checkboxDefault, checkboxPill
  , codeblock, codeblockEditable, codeblockTextArea
  , codeblockEditableBottomButton, codeblockEditableToolbarButton
  , codeblockTextAreaBottomButton, codeblockTextAreaToolbarButton
  , colorText, colorBackground, colorSamples
  , containerWithLabelDefault
  , dropdownDefault
  , loginFormTODO
  , defaultConfig, document
  , exampleLoginForm, exampleRegisterForm, exampleResetForm
  , exampleSidebar, exampleSidePanel
  , generate, generate'
  , navigationBlockDefault, navigationBlockUsageExample
  , nav, navigationNoteed, navigationNoteed'
  , radioDefaultExample, radioPillInlineExample
  , radioCheckboxExample, radioCheckboxInlineExample
  , sidePanelExample, sidePanelUsageExample
  , sidebarExample, sidebarUsageExample
  , statusCodeError400Example, statusCodeError404Example
  , tableDefault, tableCompact, tableWithColumnDivider
  , tableWithColumnDividerCompact
  , titleJumboExample, titleSubtitleJumboExample, titleDefaultExample
  , titleSubtitleDefaultExample, titleJumboUsageExample
  , titleDefaultUsageExample
  , typographyHeading1Example, typographyHeading2Example
  , typographyHeading3Example, typographyHeading4Example
  , typographyHeading5Example, typographyHeading6Example
  , typographyParagraphExample, typographyUsageExample
  , whitespaceAutoWidthExample, whitespaceNegativeMarginsExample
  , whitespaceFullWidthExample, whitespaceExamples
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

run Command.GenerateBlockquotePullQuote = generateBlockquotePullQuote

run Command.GenerateBlockquoteWithOptionalPullQuote =
  generateBlockquoteWithOptionalPullQuote

run Command.GenerateButtonPrimary = generateButtonPrimary

run Command.GenerateButtonPrimaryLarge = generateButtonPrimaryLarge

run Command.GenerateButtonPrimaryDisabled = generateButtonPrimaryDisabled

run Command.GenerateButtonSecondary = generateButtonSecondary

run Command.GenerateButtonSecondaryLarge = generateButtonSecondaryLarge

run Command.GenerateButtonSecondaryDisabled = generateButtonSecondaryDisabled

run Command.GenerateButtonFullWidth = generateButtonFullWidth

run Command.GenerateButtonLinkPrimary = generateButtonLinkPrimary

run Command.GenerateButtonLinkPrimaryLarge = generateButtonLinkPrimaryLarge

run Command.GenerateButtonLinkPrimaryDisabled = generateButtonLinkPrimaryDisabled

run Command.GenerateButtonLinkSecondary = generateButtonLinkSecondary

run Command.GenerateButtonLinkSecondaryLarge = generateButtonLinkSecondaryLarge

run Command.GenerateButtonLinkSecondaryDisabled = generateButtonLinkSecondaryDisabled

run Command.GenerateButtonLinkFullWidth = generateButtonLinkFullWidth

run Command.GenerateCheckboxDefault = generateCheckboxDefault

run Command.GenerateCheckboxPill = generateCheckboxPill

run Command.GenerateCodeblock = generateCodeblock

run Command.GenerateCodeblockWithTable = generateCodeblockWithTable

run Command.GenerateCodeblockEditable = generateCodeblockEditable

run Command.GenerateCodeblockTextArea = generateCodeblockTextArea

run Command.GenerateCodeblockEditableBottomButton =
  generateCodeblockEditableBottomButton

run Command.GenerateCodeblockEditableToolbarButton =
  generateCodeblockEditableToolbarButton

run Command.GenerateCodeblockTextAreaBottomButton =
  generateCodeblockTextAreaBottomButton

run Command.GenerateCodeblockTextAreaToolbarButton =
  generateCodeblockTextAreaToolbarButton

run Command.GenerateColorText = generateColorText

run Command.GenerateColorBackground = generateColorBackground

run Command.GenerateColorSamples = generateColorSamples

run Command.GenerateContainerWithLabel = generateContainerWithLabelDefault

run Command.GenerateDropdown = generateDropdown

run Command.GenerateFooter = generateFooter

run Command.GenerateFormLogin = generateFormLogin

run Command.GenerateNavigationBlock = generateNavigationBlock

run Command.GenerateNavigationBlockUsage = generateNavigationBlockUsage

run Command.GenerateNavigation = generateNavigation

run Command.GenerateNavigationSpaceBetween = generateNavigationSpaceBetween

run Command.GenerateRadioDefault = generateRadioDefault

run Command.GenerateRadioPillInline = generateRadioPillInline

run Command.GenerateRadioCheckbox = generateRadioCheckbox

run Command.GenerateRadioCheckboxInline = generateRadioCheckboxInline

run Command.GenerateSidePanel = generateSidePanel

run Command.GenerateSidePanelUsage = generateSidePanelUsage

run Command.GenerateSidebar = generateSidebar

run Command.GenerateSidebarUsage = generateSidebarUsage

run Command.GenerateStatusCodeError400 = generateStatusCodeError400

run Command.GenerateStatusCodeError404 = generateStatusCodeError404

run Command.GenerateTableDefault = generateTableDefault

run Command.GenerateTableCompact = generateTableCompact

run Command.GenerateTableWithColumnDivider = generateTableWithColumnDivider

run Command.GenerateTableWithColumnDividerCompact =
  generateTableWithColumnDividerCompact

run Command.GenerateTitleJumbo = generateTitleJumbo

run Command.GenerateTitleSubtitleJumbo = generateTitleSubtitleJumbo

run Command.GenerateTitle = generateTitle

run Command.GenerateTitleSubtitle = generateTitleSubtitle

run Command.GenerateTitleJumboUsage = generateTitleJumboUsage

run Command.GenerateTitleUsage = generateTitleUsage

run Command.GenerateTypographyHeading1 = generateTypographyHeading1

run Command.GenerateTypographyHeading2 = generateTypographyHeading2

run Command.GenerateTypographyHeading3 = generateTypographyHeading3

run Command.GenerateTypographyHeading4 = generateTypographyHeading4

run Command.GenerateTypographyHeading5 = generateTypographyHeading5

run Command.GenerateTypographyHeading6 = generateTypographyHeading6

run Command.GenerateTypographyParagraph = generateTypographyParagraph

run Command.GenerateTypographyUsage = generateTypographyUsage

run Command.GenerateWhitespaceAutoWidth = generateWhitespaceAutoWidth

run Command.GenerateWhitespaceNegativeMargins = generateWhitespaceNegativeMargins

run Command.GenerateWhitespaceFullWidth = generateWhitespaceFullWidth

run Command.GenerateWhitespaceExamples = generateWhitespaceExamples

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
      mapM_
        (\(cat, variants) ->
          H.li $ do
            H.text cat
            H.ul $
              mapM_
                (\(variant, href, _) ->
                  H.li $ H.a ! A.href (H.toValue href) $
                    H.text $ cat <> ", " <> variant)
                variants)
        guideData

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

  mapM_
    (\(cat, variants) ->
      mapM_
        (\(_, href, f) ->
          generate href ("Hypered style guide - " <> cat) f)
        variants)
    guideData

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
guideData :: [(Text, [(Text, FilePath, FilePath -> Html)])]
guideData =
  [
    ( "Anchor"
    , [ ("blue", "a--blue.html", const anchorBlueExample)
      , ("black", "a--black.html", const anchorBlackExample)
      ])

  , ( "Banner"
    , [ ("green", "banner--green.html", const bannerGreenExample)
      , ("red", "banner--red.html", const bannerRedExample)
      , ("yellow", "banner--yellow.html", const bannerYellowExample)
      ])

  , ( "Block quote"
    , [ ("default", "blockquote--default.html", const blockquoteDefault)
      , ("pull quote", "blockquote--pull-quote-example.html"
        , const blockquotePullQuoteExample)
      , ("optional pull quote"
        , "blockquote--with-optional-pull-quote-example.html"
        , const blockquoteWithOptionalPullQuoteExample)
      ])

  , ( "Button"
    , [ ("primary", "button--primary.html", const buttonPrimaryExample)
      , ("primary large", "button--primary-large.html"
        , const buttonPrimaryLargeExample)
      , ("primary disabled", "button--primary-disabled.html"
        , const buttonPrimaryDisabledExample)
      , ("secondary", "button--secondary.html"
        , const buttonSecondaryExample)
      , ("secondary large", "button--secondary-large.html"
        , const buttonSecondaryLargeExample)
      , ("secondary disabled", "button--secondary-disabled.html"
        , const buttonSecondaryDisabledExample)
      , ("full width", "button--full-width.html"
        , const buttonFullWidthExample)
      ])

  , ( "Button link"
    , [ ("primary", "buttonlink--primary.html", const buttonLinkPrimaryExample)
      , ("primary large", "buttonlink--primary-large.html"
        , const buttonLinkPrimaryLargeExample)
      , ("primary disabled", "buttonlink--primary-disabled.html"
        , const buttonLinkPrimaryDisabledExample)
      , ("secondary", "buttonlink--secondary.html"
        , const buttonLinkSecondaryExample)
      , ("secondary large", "buttonlink--secondary-large.html"
        , const buttonLinkSecondaryLargeExample)
      , ("secondary disabled", "buttonlink--secondary-disabled.html"
        , const buttonLinkSecondaryDisabledExample)
      , ("full width", "buttonlink--full-width.html"
        , const buttonLinkFullWidthExample)
      ])

  , ( "Checkbox"
    , [ ("default", "checkbox--default.html", const checkboxDefaultExample)
      , ("pill", "checkbox--pill.html", const checkboxPillExample)
      ])

  , ( "Code block"
    , [ ("default", "codeblock--default.html", const codeblockExample)
      , ("with-table", "codeblock--with-table.html", const codeblockWithTableExample)
      ])

  , ( "Footer"
    , [ ("default", "footer.html", const footerExample)
      ])

  , ( "Form"
    , [ ("login", "form--login.html", const loginFormTODO)
      ])

  -- This is mostly header / nav / a, a, ...
  , ( "Navigation"
    , [ ("default", "navigation.html", navigation)
      ])

  , ( "Table"
    , [ ("default", "table--default.html", const tableDefaultExample)
      , ("compact", "table--compact.html", const tableCompactExample)
      , ("with column divider", "table--with-column-divider.html"
        , const tableWithColumnDividerExample)
      , ("with column divider, compact"
        , "table--with-column-divider-compact.html"
        , const tableWithColumnDividerCompactExample)
      ])
  ]


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

generateBlockquotePullQuote :: IO ()
generateBlockquotePullQuote = putStr $ renderHtml blockquotePullQuoteExample

generateBlockquoteWithOptionalPullQuote :: IO ()
generateBlockquoteWithOptionalPullQuote =
  putStr $ renderHtml blockquoteWithOptionalPullQuoteExample

generateButtonPrimary :: IO ()
generateButtonPrimary = putStr $ renderHtml buttonPrimaryExample

generateButtonPrimaryLarge :: IO ()
generateButtonPrimaryLarge = putStr $ renderHtml buttonPrimaryLargeExample

generateButtonPrimaryDisabled :: IO ()
generateButtonPrimaryDisabled = putStr $ renderHtml buttonPrimaryDisabledExample

generateButtonSecondary :: IO ()
generateButtonSecondary = putStr $ renderHtml buttonSecondaryExample

generateButtonSecondaryLarge :: IO ()
generateButtonSecondaryLarge = putStr $ renderHtml buttonSecondaryLargeExample

generateButtonSecondaryDisabled :: IO ()
generateButtonSecondaryDisabled = putStr $ renderHtml buttonSecondaryDisabledExample

generateButtonFullWidth :: IO ()
generateButtonFullWidth = putStr $ renderHtml buttonFullWidthExample

generateButtonLinkPrimary :: IO ()
generateButtonLinkPrimary = putStr $ renderHtml buttonLinkPrimaryExample

generateButtonLinkPrimaryLarge :: IO ()
generateButtonLinkPrimaryLarge = putStr $ renderHtml buttonLinkPrimaryLargeExample

generateButtonLinkPrimaryDisabled :: IO ()
generateButtonLinkPrimaryDisabled = putStr $ renderHtml buttonLinkPrimaryDisabledExample

generateButtonLinkSecondary :: IO ()
generateButtonLinkSecondary = putStr $ renderHtml buttonLinkSecondaryExample

generateButtonLinkSecondaryLarge :: IO ()
generateButtonLinkSecondaryLarge = putStr $ renderHtml buttonLinkSecondaryLargeExample

generateButtonLinkSecondaryDisabled :: IO ()
generateButtonLinkSecondaryDisabled = putStr $ renderHtml buttonLinkSecondaryDisabledExample

generateButtonLinkFullWidth :: IO ()
generateButtonLinkFullWidth = putStr $ renderHtml buttonLinkFullWidthExample

generateCheckboxDefault :: IO ()
generateCheckboxDefault = putStr $ renderHtml checkboxDefaultExample

generateCheckboxPill :: IO ()
generateCheckboxPill = putStr $ renderHtml checkboxPillExample

generateCodeblock :: IO ()
generateCodeblock = putStr $ renderHtml codeblockExample

generateCodeblockWithTable :: IO ()
generateCodeblockWithTable = putStr $ renderHtml codeblockWithTableExample

generateCodeblockEditable :: IO ()
generateCodeblockEditable = putStr $ renderHtml codeblockEditableExample

generateCodeblockTextArea :: IO ()
generateCodeblockTextArea = putStr $ renderHtml codeblockTextAreaExample

generateCodeblockEditableBottomButton :: IO ()
generateCodeblockEditableBottomButton =
  putStr $ renderHtml codeblockEditableBottomButtonExample

generateCodeblockEditableToolbarButton :: IO ()
generateCodeblockEditableToolbarButton =
  putStr $ renderHtml codeblockEditableToolbarButtonExample

generateCodeblockTextAreaBottomButton :: IO ()
generateCodeblockTextAreaBottomButton =
  putStr $ renderHtml codeblockTextAreaBottomButtonExample

generateCodeblockTextAreaToolbarButton :: IO ()
generateCodeblockTextAreaToolbarButton =
  putStr $ renderHtml codeblockTextAreaToolbarButtonExample

generateColorText :: IO ()
generateColorText = putStr $ renderHtml colorTextExample

generateColorBackground :: IO ()
generateColorBackground = putStr $ renderHtml colorBackgroundExample

generateColorSamples :: IO ()
generateColorSamples = putStr $ renderHtml colorSamplesExample

generateContainerWithLabelDefault :: IO ()
generateContainerWithLabelDefault =
  putStr $ renderHtml containerWithLabelDefaultExample

generateDropdown :: IO ()
generateDropdown = putStr $ renderHtml dropdownDefaultExample

generateFooter :: IO ()
generateFooter = putStr $ renderHtml footerExample

generateFormLogin :: IO ()
generateFormLogin = putStr $ renderHtml loginFormTODO

generateNavigationBlock :: IO ()
generateNavigationBlock = putStr $ renderHtml navigationBlockDefault

generateNavigationBlockUsage :: IO ()
generateNavigationBlockUsage =
  putStr $ renderHtml navigationBlockUsageExample

generateNavigation :: IO ()
generateNavigation = putStr $ renderHtml navigationNoteed

generateNavigationSpaceBetween :: IO ()
generateNavigationSpaceBetween = putStr $ renderHtml navigationNoteed'

generateRadioDefault :: IO ()
generateRadioDefault = putStr $ renderHtml radioDefaultExample

generateRadioPillInline :: IO ()
generateRadioPillInline = putStr $ renderHtml radioPillInlineExample

generateRadioCheckbox :: IO ()
generateRadioCheckbox = putStr $ renderHtml radioCheckboxExample

generateRadioCheckboxInline :: IO ()
generateRadioCheckboxInline = putStr $ renderHtml radioCheckboxInlineExample

generateSidePanel :: IO ()
generateSidePanel = putStr $ renderHtml sidePanelExample

generateSidePanelUsage :: IO ()
generateSidePanelUsage = putStr $ renderHtml sidePanelUsageExample

generateSidebar :: IO ()
generateSidebar = putStr $ renderHtml sidebarExample

generateSidebarUsage :: IO ()
generateSidebarUsage = putStr $ renderHtml sidebarUsageExample

generateStatusCodeError400 :: IO ()
generateStatusCodeError400 = putStr $ renderHtml statusCodeError400Example

generateStatusCodeError404 :: IO ()
generateStatusCodeError404 = putStr $ renderHtml statusCodeError404Example

generateTableDefault :: IO ()
generateTableDefault = putStr $ renderHtml tableDefaultExample

generateTableCompact :: IO ()
generateTableCompact = putStr $ renderHtml tableCompactExample

generateTableWithColumnDivider :: IO ()
generateTableWithColumnDivider =
  putStr $ renderHtml tableWithColumnDividerExample

generateTableWithColumnDividerCompact :: IO ()
generateTableWithColumnDividerCompact =
  putStr $ renderHtml tableWithColumnDividerCompactExample

generateTitleJumbo :: IO ()
generateTitleJumbo = putStr $ renderHtml titleJumboExample

generateTitleSubtitleJumbo :: IO ()
generateTitleSubtitleJumbo = putStr $ renderHtml titleSubtitleJumboExample

generateTitle :: IO ()
generateTitle = putStr $ renderHtml titleDefaultExample

generateTitleSubtitle :: IO ()
generateTitleSubtitle = putStr $ renderHtml titleSubtitleDefaultExample

generateTitleJumboUsage :: IO ()
generateTitleJumboUsage = putStr $ renderHtml titleJumboUsageExample

generateTitleUsage :: IO ()
generateTitleUsage = putStr $ renderHtml titleDefaultUsageExample

generateTypographyHeading1 :: IO ()
generateTypographyHeading1 = putStr $ renderHtml typographyHeading1Example

generateTypographyHeading2 :: IO ()
generateTypographyHeading2 = putStr $ renderHtml typographyHeading2Example

generateTypographyHeading3 :: IO ()
generateTypographyHeading3 = putStr $ renderHtml typographyHeading3Example

generateTypographyHeading4 :: IO ()
generateTypographyHeading4 = putStr $ renderHtml typographyHeading4Example

generateTypographyHeading5 :: IO ()
generateTypographyHeading5 = putStr $ renderHtml typographyHeading5Example

generateTypographyHeading6 :: IO ()
generateTypographyHeading6 = putStr $ renderHtml typographyHeading6Example

generateTypographyParagraph :: IO ()
generateTypographyParagraph = putStr $ renderHtml typographyParagraphExample

generateTypographyUsage :: IO ()
generateTypographyUsage = putStr $ renderHtml typographyUsageExample

generateWhitespaceAutoWidth :: IO ()
generateWhitespaceAutoWidth = putStr $ renderHtml whitespaceAutoWidthExample

generateWhitespaceNegativeMargins :: IO ()
generateWhitespaceNegativeMargins = putStr $ renderHtml whitespaceNegativeMarginsExample

generateWhitespaceFullWidth :: IO ()
generateWhitespaceFullWidth = putStr $ renderHtml whitespaceFullWidthExample

generateWhitespaceExamples :: IO ()
generateWhitespaceExamples = putStr $ renderHtml whitespaceExamples


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


--------------------------------------------------------------------------------
buttonPrimaryExample :: Html
buttonPrimaryExample = buttonPrimary "Primary Button"

buttonPrimaryLargeExample :: Html
buttonPrimaryLargeExample = buttonPrimaryLarge "Primary Button"

buttonPrimaryDisabledExample :: Html
buttonPrimaryDisabledExample = buttonPrimaryDisabled "Primary Disabled"

buttonSecondaryExample :: Html
buttonSecondaryExample = buttonSecondary "Secondary Button"

buttonSecondaryLargeExample :: Html
buttonSecondaryLargeExample = buttonSecondaryLarge "Secondary Button"

buttonSecondaryDisabledExample :: Html
buttonSecondaryDisabledExample = buttonSecondaryDisabled "Secondary Disabled"

buttonFullWidthExample :: Html
buttonFullWidthExample = buttonFullWidth "Button Full Width"


--------------------------------------------------------------------------------
buttonLinkPrimaryExample :: Html
buttonLinkPrimaryExample = buttonLinkPrimary "Primary Button"

buttonLinkPrimaryLargeExample :: Html
buttonLinkPrimaryLargeExample = buttonLinkPrimaryLarge "Primary Button"

buttonLinkPrimaryDisabledExample :: Html
buttonLinkPrimaryDisabledExample = buttonLinkPrimaryDisabled "Primary Disabled"

buttonLinkSecondaryExample :: Html
buttonLinkSecondaryExample = buttonLinkSecondary "Secondary Button"

buttonLinkSecondaryLargeExample :: Html
buttonLinkSecondaryLargeExample = buttonLinkSecondaryLarge "Secondary Button"

buttonLinkSecondaryDisabledExample :: Html
buttonLinkSecondaryDisabledExample = buttonLinkSecondaryDisabled "Secondary Disabled"

buttonLinkFullWidthExample :: Html
buttonLinkFullWidthExample = buttonLinkFullWidth "Button Full Width"


--------------------------------------------------------------------------------
checkboxDefaultExample :: Html
checkboxDefaultExample = checkboxDefault "This is checked"

checkboxPillExample :: Html
checkboxPillExample = checkboxPill "This is checked"


--------------------------------------------------------------------------------
codeblockExample :: Html
codeblockExample = codeblock
  "// this is a comment\n\
  \// this is another comment\n\
  \// this is a slightly longer comment\n"

codeblockWithTableExample :: Html
codeblockWithTableExample = codeblock
  "\n\
  \// table examples from: https://ozh.github.io/ascii-tables/\n\
  \\n\
  \// example 1:\n\
  \┌──────────────────────────────────┬─────────┬────────────────────────┬────────────────┐\n\
  \│               Col1               │  Col2   │          Col3          │ Numeric Column │\n\
  \├──────────────────────────────────┼─────────┼────────────────────────┼────────────────┤\n\
  \│ Value 1                          │ Value 2 │ 123                    │           10.0 │\n\
  \│ Separate                         │ cols    │ with a tab or 4 spaces │       -2,027.1 │\n\
  \│ This is a row with only one cell │         │                        │                │\n\
  \└──────────────────────────────────┴─────────┴────────────────────────┴────────────────┘\n\
  \\n\
  \// example 2:\n\
  \\n\
  \|               Col1               |  Col2   |          Col3          | Numeric Column |\n\
  \|----------------------------------|---------|------------------------|----------------|\n\
  \| Value 1                          | Value 2 | 123                    |           10.0 |\n\
  \| Separate                         | cols    | with a tab or 4 spaces |       -2,027.1 |\n\
  \| This is a row with only one cell |         |                        |                |\n"

codeblockEditableExample :: Html
codeblockEditableExample = codeblockEditable
  "// This block of code can be edited.\n\
  \// this is a comment\n\
  \// this is another comment\n\
  \// this is a slightly longer comment\n"

codeblockTextAreaExample :: Html
codeblockTextAreaExample = codeblockTextArea
  "// This is an example of a block of code that can be edited.\n\
  \// this is a comment\n\
  \// this is another comment\n\
  \// this is a slightly longer comment\n"

codeblockEditableBottomButtonExample :: Html
codeblockEditableBottomButtonExample = codeblockEditableBottomButton
  "// This block of code can be edited.\n\
  \// this is a comment\n\
  \// this is another comment\n\
  \// this is a slightly longer comment\n"

codeblockEditableToolbarButtonExample :: Html
codeblockEditableToolbarButtonExample = codeblockEditableToolbarButton
  "// This block of code can be edited.\n\
  \// this is a comment\n\
  \// this is another comment\n\
  \// this is a slightly longer comment\n"

codeblockTextAreaBottomButtonExample :: Html
codeblockTextAreaBottomButtonExample = codeblockTextAreaBottomButton
  "// This block of code can be edited.\n\
  \// this is a comment\n\
  \// this is another comment\n\
  \// this is a slightly longer comment\n"

codeblockTextAreaToolbarButtonExample :: Html
codeblockTextAreaToolbarButtonExample = codeblockTextAreaToolbarButton
  "// This block of code can be edited.\n\
  \// this is a comment\n\
  \// this is another comment\n\
  \// this is a slightly longer comment\n"


--------------------------------------------------------------------------------
colorTextExample :: Html
colorTextExample = colorText

colorBackgroundExample :: Html
colorBackgroundExample = colorBackground

colorSamplesExample :: Html
colorSamplesExample = colorSamples


--------------------------------------------------------------------------------
containerWithLabelDefaultExample :: Html
containerWithLabelDefaultExample = containerWithLabelDefault


--------------------------------------------------------------------------------
dropdownDefaultExample :: Html
dropdownDefaultExample = dropdownDefault


--------------------------------------------------------------------------------
footerExample :: Html
footerExample = footer "© Hypered, 2019-2023."

tableDefaultExample :: Html
tableDefaultExample = tableDefault -- TODO Arguments.

tableCompactExample :: Html
tableCompactExample = tableCompact

tableWithColumnDividerExample :: Html
tableWithColumnDividerExample = tableWithColumnDivider

tableWithColumnDividerCompactExample :: Html
tableWithColumnDividerCompactExample = tableWithColumnDividerCompact


------------------------------------------------------------------------------
-- Stories from Storybook

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
