{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypered.Design.Run
  ( run
  ) where

import           Data.List (nub, tail)
import qualified Data.Text                     as T
import qualified Hypered.Design.Command        as Command
import           Hypered.Design.Examples
import qualified Hypered.Design.Server         as Server
import           Hypered.Design.Stories         ( stories )
import           Hypered.Design.Templates       ( generateTemplates )
import           Hypered.Html
  ( Font(Inter)
  , generate, generate'
  , Config(..)
  , defaultConfig, document

  , loginForm, loginFormReesd
  , imageExamples
  , inputTextExample, inputPasswordExample, inputNumberExample
  , inputWithMessageExample, inputUsage
  , listOrderedExample, listUnorderedExample
  , exampleLoginForm, exampleRegisterForm, exampleResetForm
  , exampleSidebar, exampleSidePanel
  , radioDefaultExample, radioPillInlineExample
  , radioCheckboxExample, radioCheckboxInlineExample
  , sidePanelExample, sidePanelUsageExample
  , sidebarExample, sidebarUsageExample
  , statusCodeError400Example, statusCodeError404Example
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
import           Protolude
import           Text.Blaze.Html5 ((!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)
import           Text.Blaze.Html.Renderer.Text (renderHtml)


--------------------------------------------------------------------------------
run :: Command.Command -> IO ()
run (Command.GenerateTemplates forGitHubPages) = generateTemplates forGitHubPages

run Command.GenerateGuide = generateGuide

run Command.Wrapper = generateWrapper

run (Command.Serve conf) = Server.run conf

run Command.GenerateAnchorBlue = generateAnchorBlue

run Command.GenerateAnchorBlack = generateAnchorBlack

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

run Command.GenerateHorizontalRule = generateHr

run Command.GenerateHorizontalRuleDivider = generateHrDivider

run Command.GenerateImage = generateImage

run Command.GenerateImageNegativePull = generateImageNegativePull

run Command.GenerateImageFullWidth = generateImageFullWidth

run Command.GenerateImageWithCaption = generateImageWithCaption

run Command.GenerateImageExamples = generateImageExamples

run Command.GenerateInputText = generateInputText

run Command.GenerateInputPassword = generateInputPassword

run Command.GenerateInputNumber = generateInputNumber

run Command.GenerateInputWithMessage = generateInputWithMessage

run Command.GenerateInputUsage = generateInputUsage

run Command.GenerateLayout = generateLayout

run Command.GenerateLayoutBlogList = generateLayoutBlogList

run Command.GenerateLayoutBlogPost1 = generateLayoutBlogPost1

run Command.GenerateLayoutBlogPost2 = generateLayoutBlogPost2

run Command.GenerateLayoutWithSidebar = generateLayoutWithSidebar

run Command.GenerateListOrdered = generateListOrdered

run Command.GenerateListUnordered = generateListUnordered

run Command.GenerateModalTextContent = generateModalTextContent

run Command.GenerateModalButtonLabel = generateModalButtonLabel

run Command.GenerateModalTextLabel = generateModalTextLabel

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

run Command.ListCategories = listCategories

run Command.ListStories = listStories

run Command.JsImportStories = jsImportStories

run Command.JsStories = jsStories


------------------------------------------------------------------------------
generateGuide :: IO ()
generateGuide = do
  let conf = defaultConfig { cAddWrapper = False }

  generate' "index.html" "Hypered style guide" conf $ \_ ->
    H.div ! A.class_ "flex flex-column justify-between min-height-vh-100 mw8 center pa3 pa4-ns lh-copy" $ do
      H.h1 "Components and examples"

      H.h2 "Components"
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

      H.h2 "Examples"
      H.ul $ do
        H.li $ H.a ! A.href "example--login-form.html" $ "Example, login form"
        H.li $ H.a ! A.href "example--sidebar.html" $ "Example, sidebar"
        H.li $ H.a ! A.href "example--side-panel.html" $ "Example, side panel"
        H.li $ H.a ! A.href "example--blog-post.html" $ "Example, blog post"

        H.li $ H.a ! A.href "example--template.html" $ "Example, template"

        H.li $ H.a ! A.href "example--login-form-inter.html" $
          "Example, login form (Inter)"
        H.li $ H.a ! A.href "example--register-form-inter.html" $
          "Example, register form (Inter)"
        H.li $ H.a ! A.href "example--reset-form-inter.html" $
          "Example, reset form (Inter)"
        H.li $ H.a ! A.href "example--sidebar-inter.html" $
          "Example, sidebar (Inter)"
        H.li $ H.a ! A.href "example--side-panel-inter.html" $
          "Example, side panel (Inter)"
        H.li $ H.a ! A.href "example--blog-post-inter.html" $
          "Example, blog post (Inter)"

        H.li $ H.a ! A.href "example--template-inter.html" $
          "Example, template (Inter)"

  mapM_
    (\(cat, variants) ->
      mapM_
        (\(_, href, f) ->
          generate href ("Hypered style guide - " <> cat) f)
        variants)
    guideData

  -- Example usage

  generate' "example--login-form.html"
    "Hypered style guide - Login form eExample"
    conf (const exampleLoginForm)
  generate' "example--sidebar.html"
    "Hypered style guide - Sidebar example"
    conf (const exampleSidebar)
  generate' "example--side-panel.html"
    "Hypered style guide - Side panel example"
    conf (const exampleSidePanel)
  generate' "example--blog-post.html"
    "Hypered style guide - Blog post example"
    conf (const layoutBlogPost1Example)

  -- Example usage using Inter

  let conf' = conf { cFont = Inter }
  generate' "example--login-form-inter.html"
    "Hypered style guide - Login form example"
    conf' (const exampleLoginForm)
  generate' "example--register-form-inter.html"
    "Hypered style guide - Register form example"
    conf' (const exampleRegisterForm)
  generate' "example--reset-form-inter.html"
    "Hypered style guide - Reset form example"
    conf' (const exampleResetForm)
  generate' "example--sidebar-inter.html"
    "Hypered style guide - Sidebar example"
    conf' (const exampleSidebar)
  generate' "example--side-panel-inter.html"
    "Hypered style guide - Side panel example"
    conf' (const exampleSidePanel)
  generate' "example--blog-post-inter.html"
    "Hypered style guide - Blog post example"
    conf' (const layoutBlogPost1Example)


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
      , ("with-table", "codeblock--with-table.html"
        , const codeblockWithTableExample)
      , ("editable", "codeblock--editable.html", const codeblockEditableExample)
      , ("textarea", "codeblock--textarea.html", const codeblockTextAreaExample)
      , ("editable with bottom button", "codeblock--editable-bottom-button.html"
        , const codeblockEditableBottomButtonExample)
      , ("editable with toolbar button"
        , "codeblock--editable-toolbar-button.html"
        , const codeblockEditableToolbarButtonExample)
      , ("textarea with bottom button", "codeblock--textarea-bottom-button.html"
        , const codeblockTextAreaBottomButtonExample)
      , ("textarea with toolbar button"
        , "codeblock--textarea-toolbar-button.html"
        , const codeblockTextAreaToolbarButtonExample)
      ])

  , ( "Colour"
    , [ ("text", "colour--text.html", const colorTextExample)
      , ("background", "colour--background.html", const colorBackgroundExample)
      , ("smaples", "colour--samples.html", const colorSamplesExample)
      ])

  , ( "Container with label"
    , [ ("default", "container-with-label--default.html"
        , const containerWithLabelDefaultExample)
      ])

  , ( "Dropdown"
    , [ ("default", "dropdown--default.html", const dropdownDefaultExample)
      ])

  , ( "Footer"
    , [ ("default", "footer--default.html", const footerExample)
      ])

  , ( "Form"
    , [ ("login", "form--login.html", const loginForm)
      , ("login (Reesd)", "form--login-reesd.html", const loginFormReesd)
      ])

  , ( "Horizontal rule"
    , [ ("default", "hr--default.html", const hrExample)
      , ("divider", "hr--divider.html", const hrDividerExample)
      ])

  , ( "Image"
    , [ ("default", "image--default.html", const imageExample)
      , ("negative pull", "image--negative-pull.html"
        , const imageNegativePullExample)
      , ("full width", "image--full-width.html", const imageFullWidthExample)
      , ("with caption", "image--with-caption.html"
        , const imageWithCaptionExample)
      , ("examples", "image--examples.html", const imageExamples)
      ])

  , ( "Input"
    , [ ("text", "input--text.html", const inputTextExample)
      , ("password", "input--password.html", const inputPasswordExample)
      , ("number", "input--number.html", const inputNumberExample)
      , ("with message", "input--with-message.html"
        , const inputWithMessageExample)
      , ("usage", "input--usage.html", const inputUsage)
      ])

  , ( "Layout"
    , [ ("default", "layout--default.html", const layoutExample)
      , ("blog list", "layout--blog-list.html", const layoutBlogListExample)
      , ("blog post 1", "layout--blog-post-1.html"
        , const layoutBlogPost1Example)
      , ("blog post 2", "layout--blog-post-2.html"
        , const layoutBlogPost2Example)
      , ("with sidebar", "layout--with-sidebar.html", const layoutWithSidebarExample)
      ])

  , ( "List"
    , [ ("ordered", "list--ordered.html", const listOrderedExample)
      , ("unordered", "list--unordered.html", const listUnorderedExample)
      ])

  , ( "Modal"
    , [ ("text-content", "modal--text-content.html"
        , const modalTextContentExample)
      , ("button-label", "modal--button-label.html"
        , const modalButtonLabelExample)
      , ("text-label", "modal--text-label.html"
        , const modalTextLabelExample)
      ])

  , ( "Navigation block"
    , [ ("default", "navigation-block--default.html"
        , const navigationBlockExample)
      , ("usage", "navigation-block--usage.html"
        , const navigationBlockUsageExample)
      ])

  , ( "Navigation"
    , [ ("default", "navigation--default.html", const navigationExample)
      , ("space between", "navigation--space-between.html"
        , const navigationSpaceBetweenExample)
      ])

  , ( "Radio"
    , [ ("pill", "radio--pill.html", const radioDefaultExample)
      , ("checkbox", "radio--checkbox.html", const radioCheckboxExample)
      , ("pill-inline", "radio--pill-inline.html", const radioPillInlineExample)
      , ("checkbox-inline", "radio--checkbox-inline.html"
        , const radioCheckboxInlineExample)
      ])

  , ( "Side panel"
    , [ ("default", "side-panel--default.html", const sidePanelExample)
      , ("usage", "side-panel--usage.html", const sidePanelUsageExample)
      ])

  , ( "Sidebar"
    , [ ("default", "sidebar--default.html", const sidebarExample)
      , ("usage", "sidebar--usage.html", const sidebarUsageExample)
      ])

  , ( "Status code"
    , [ ("error-400", "status-code--error-400.html"
        , const statusCodeError400Example)
      , ("error-404", "status-code--error-404.html"
        , const statusCodeError404Example)
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

  , ( "Title"
    , [ ("default", "title--default.html", const titleDefaultExample)
      , ("subtitle", "title--subtitle.html", const titleSubtitleDefaultExample)
      , ("jumbo", "title--jumbo.html", const titleJumboExample)
      , ("subtitle-jumbo", "title--subtitle-jumbo.html"
        , const titleSubtitleJumboExample)
      , ("usage", "title--usage.html", const titleDefaultUsageExample)
      , ("jumbo-usage", "title--jumbo-usage.html", const titleJumboUsageExample)
      ])

  , ( "Typography"
    , [ ("heading 1", "typography--heading-1.html"
        , const typographyHeading1Example)
      , ("heading 2", "typography--heading-2.html"
        , const typographyHeading2Example)
      , ("heading 3", "typography--heading-3.html"
        , const typographyHeading3Example)
      , ("heading 4", "typography--heading-4.html"
        , const typographyHeading4Example)
      , ("heading 5", "typography--heading-5.html"
        , const typographyHeading5Example)
      , ("heading 6", "typography--heading-6.html"
        , const typographyHeading6Example)
      , ("paragraph", "typography--paragraph.html"
        , const typographyParagraphExample)
      , ("usage", "typography--usage.html"
        , const typographyUsageExample)
      ])

  , ( "Whitespace"
    , [ ("auto width", "whitespace--auto-width.html"
        , const whitespaceAutoWidthExample)
      , ("negative margins", "whitespace--negative-margins.html"
        , const whitespaceNegativeMarginsExample)
      , ("full width", "whitespace--full-width.html"
        , const whitespaceFullWidthExample)
      , ("examples", "whitespace--examples.html"
        , const whitespaceExamples)
      ])
  ]


------------------------------------------------------------------------------
-- The document wrapper. This should match `pages/_app.js`.
generateWrapper :: IO ()
generateWrapper = do
  putStr (Pretty.renderHtml
    ( document defaultConfig "wrapper.html" "Hypered design system" $
        H.div ! A.class_ "flex flex-column justify-between min-height-vh-100 mw8 center pa3 pa4-ns lh-copy" $
          H.preEscapedToHtml @Text "<!-- CONTENT MARKER -->"
    ))


------------------------------------------------------------------------------
-- Individual components

generateAnchorBlue :: IO ()
generateAnchorBlue = putStr $ renderHtml anchorBlueExample

generateAnchorBlack :: IO ()
generateAnchorBlack = putStr $ renderHtml anchorBlackExample

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
generateFormLogin = putStr $ renderHtml loginForm

generateHr :: IO ()
generateHr = putStr $ renderHtml hrExample

generateHrDivider :: IO ()
generateHrDivider = putStr $ renderHtml hrDividerExample

generateImage :: IO ()
generateImage = putStr $ renderHtml imageExample

generateImageNegativePull :: IO ()
generateImageNegativePull = putStr $ renderHtml imageNegativePullExample

generateImageFullWidth :: IO ()
generateImageFullWidth = putStr $ renderHtml imageFullWidthExample

generateImageWithCaption :: IO ()
generateImageWithCaption = putStr $ renderHtml imageWithCaptionExample

generateImageExamples :: IO ()
generateImageExamples = putStr $ renderHtml imageExamples

generateInputText :: IO ()
generateInputText = putStr $ renderHtml inputTextExample

generateInputPassword :: IO ()
generateInputPassword = putStr $ renderHtml inputPasswordExample

generateInputNumber :: IO ()
generateInputNumber = putStr $ renderHtml inputNumberExample

generateInputWithMessage :: IO ()
generateInputWithMessage = putStr $ renderHtml inputWithMessageExample

generateInputUsage :: IO ()
generateInputUsage = putStr $ renderHtml inputUsage

generateLayout :: IO ()
generateLayout = putStr $ renderHtml layoutExample

generateLayoutBlogList :: IO ()
generateLayoutBlogList = putStr $ renderHtml layoutBlogListExample

generateLayoutBlogPost1 :: IO ()
generateLayoutBlogPost1 = putStr $ renderHtml layoutBlogPost1Example

generateLayoutBlogPost2 :: IO ()
generateLayoutBlogPost2 = putStr $ renderHtml layoutBlogPost2Example

generateLayoutWithSidebar :: IO ()
generateLayoutWithSidebar = putStr $ renderHtml layoutWithSidebarExample

generateListOrdered :: IO ()
generateListOrdered = putStr $ renderHtml listOrderedExample

generateListUnordered :: IO ()
generateListUnordered = putStr $ renderHtml listUnorderedExample

generateModalTextContent :: IO ()
generateModalTextContent = putStr $ renderHtml modalTextContentExample

generateModalButtonLabel :: IO ()
generateModalButtonLabel = putStr $ renderHtml modalButtonLabelExample

generateModalTextLabel :: IO ()
generateModalTextLabel = putStr $ renderHtml modalTextLabelExample

generateNavigationBlock :: IO ()
generateNavigationBlock = putStr $ renderHtml navigationBlockExample

generateNavigationBlockUsage :: IO ()
generateNavigationBlockUsage =
  putStr $ renderHtml navigationBlockUsageExample

generateNavigation :: IO ()
generateNavigation = putStr $ renderHtml navigationExample

generateNavigationSpaceBetween :: IO ()
generateNavigationSpaceBetween =
  putStr $ renderHtml navigationSpaceBetweenExample

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
