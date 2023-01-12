{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Hypered.Design.Command
  ( Command(..)
  , parserInfo
  ) where

import qualified Options.Applicative           as A
import           Protolude


--------------------------------------------------------------------------------
data Command =
    GenerateGuide
  | GenerateTemplates Bool
    -- ^ If True, use a prefix suitable for GitHub Pages (i.e. the /design
    -- prefix, since the static site is at hypered.github.io/design).
  | Wrapper
    -- ^ The document wrapper. This should match `pages/_app.js`.

    -- Helpers to explore Storybook stories

  | ListCategories
  | ListStories
  | JsImportStories
  | JsStories

    -- Individual components

  | GenerateAnchorBlue
  | GenerateAnchorBlack

  | GenerateBannerGreen
  | GenerateBannerRed
  | GenerateBannerYellow

  | GenerateBlockquoteDefault
  | GenerateBlockquotePullQuote
  | GenerateBlockquoteWithOptionalPullQuote

  | GenerateButtonPrimary
  | GenerateButtonPrimaryLarge
  | GenerateButtonPrimaryDisabled
  | GenerateButtonSecondary
  | GenerateButtonSecondaryLarge
  | GenerateButtonSecondaryDisabled
  | GenerateButtonFullWidth

  | GenerateButtonLinkPrimary
  | GenerateButtonLinkPrimaryLarge
  | GenerateButtonLinkPrimaryDisabled
  | GenerateButtonLinkSecondary
  | GenerateButtonLinkSecondaryLarge
  | GenerateButtonLinkSecondaryDisabled
  | GenerateButtonLinkFullWidth

  | GenerateCheckboxDefault
  | GenerateCheckboxPill

  | GenerateCodeblock
  | GenerateCodeblockWithTable
  | GenerateCodeblockEditable
  | GenerateCodeblockTextArea
  | GenerateCodeblockEditableBottomButton
  | GenerateCodeblockEditableToolbarButton
  | GenerateCodeblockTextAreaBottomButton
  | GenerateCodeblockTextAreaToolbarButton

  | GenerateColorText
  | GenerateColorBackground
  | GenerateColorSamples

  | GenerateContainerWithLabel

  | GenerateDropdown

  | GenerateFooter

  | GenerateFormLogin

  | GenerateHorizontalRule
  | GenerateHorizontalRuleDivider

  | GenerateImage
  | GenerateImageNegativePull
  | GenerateImageFullWidth
  | GenerateImageWithCaption
  | GenerateImageExamples

  | GenerateInputText
  | GenerateInputPassword
  | GenerateInputNumber
  | GenerateInputWithMessage
  | GenerateInput

  | GenerateLayout
  | GenerateLayoutBlogList
  | GenerateLayoutBlogPost
  | GenerateLayoutWithSidebar

  | GenerateListOrdered
  | GenerateListUnordered

  -- TODO Modal.

  | GenerateNavigationBlock
  | GenerateNavigationBlockUsage

  | GenerateNav
  | GenerateNavigation
  | GenerateNavigationSpaceBetween

  | GenerateRadioDefault
  | GenerateRadioPillInline
  | GenerateRadioCheckbox
  | GenerateRadioCheckboxInline

  | GenerateSidePanel
  | GenerateSidePanelUsage

  | GenerateSidebar
  | GenerateSidebarUsage

  | GenerateStatusCodeError400
  | GenerateStatusCodeError404

  | GenerateTableDefault
  | GenerateTableCompact
  | GenerateTableWithColumnDivider
  | GenerateTableWithColumnDividerCompact

  | GenerateTitleJumbo
  | GenerateTitleSubtitleJumbo
  | GenerateTitle
  | GenerateTitleSubtitle
  | GenerateTitleJumboUsage
  | GenerateTitleUsage

  | GenerateTypographyHeading1
  | GenerateTypographyHeading2
  | GenerateTypographyHeading3
  | GenerateTypographyHeading4
  | GenerateTypographyHeading5
  | GenerateTypographyHeading6
  | GenerateTypographyParagraph
  | GenerateTypographyUsage

  | GenerateWhitespaceAutoWidth
  | GenerateWhitespaceNegativeMargins
  | GenerateWhitespaceFullWidth
  | GenerateWhitespaceExamples

    -- Stories from Storybook

  | GenerateLayoutDefault
  deriving (Eq, Show)


--------------------------------------------------------------------------------
parserInfo :: A.ParserInfo Command
parserInfo =
  A.info (parser <**> A.helper)
    $  A.fullDesc
    <> A.header "hypered-design - The source of truth for the Hypered design system."
    <> A.progDesc
         "This is the Haskell implementation of the Hypered design system. \
         \It is possible to generate static HTML pages or display individual \
         \components (this is used to compare with the reference Next.js \
         \implementation)."


--------------------------------------------------------------------------------
parser :: A.Parser Command
parser =
  A.subparser
      (  A.command
          "generate-guide"
          ( A.info (parserGenerateGuide <**> A.helper)
          $ A.progDesc "Generate HTML pages to showcase the components"
          )

      <> A.command
          "generate-templates"
          ( A.info (parserGenerateTemplates <**> A.helper)
          $ A.progDesc "Generate HTML templates that can be used with Pandoc"
          )

      <> A.command
          "wrapper"
          ( A.info (parserWrapper <**> A.helper)
          $ A.progDesc "Generate the document wrapper. This should match `pages/_app.js`."
          )
     )

  <|> A.subparser
      (  A.commandGroup "Storybook"

      <> A.command
          "list-categories"
          ( A.info (parserListCategories <**> A.helper)
          $ A.progDesc "List Storybook categories"
          )

      <> A.command
          "list-stories"
          ( A.info (parserListStories <**> A.helper)
          $ A.progDesc "List Storybook stories"
          )

      <> A.command
          "js-import-stories"
          ( A.info (parserJsImportStories <**> A.helper)
          $ A.progDesc "Generate JS code to import the Storybook stories"
          )

      <> A.command
          "js-stories"
          ( A.info (parserJsStories <**> A.helper)
          $ A.progDesc "Generate JS code to render Storybook stories"
          )
      )

  <|> A.subparser
      (  A.commandGroup "Components"

      -- Anchor
      <> A.command
          "a--blue"
          ( A.info (parserAnchorBlue <**> A.helper)
          $ A.progDesc "Generate a blue link component"
          )
      <> A.command
          "a--black"
          ( A.info (parserAnchorBlack <**> A.helper)
          $ A.progDesc "Generate a black link component"
          )

      -- Banner
      <> A.command
          "banner--green"
          ( A.info (parserBannerGreen <**> A.helper)
          $ A.progDesc "Generate a green banner component"
          )
      <> A.command
          "banner--red"
          ( A.info (parserBannerRed <**> A.helper)
          $ A.progDesc "Generate a red banner component"
          )
      <> A.command
          "banner--yellow"
          ( A.info (parserBannerYellow <**> A.helper)
          $ A.progDesc "Generate a yellow banner component"
          )

      -- Blockquote
      <> A.command
          "blockquote--default"
          ( A.info (parserBlockquoteDefault <**> A.helper)
          $ A.progDesc "Generate a blockquote component"
          )
      <> A.command
          "blockquote--pull-quote"
          ( A.info (parserBlockquotePullQuote <**> A.helper)
          $ A.progDesc "Generate a pull quote component"
          )
      <> A.command
          "blockquote--with-optional-pull-quote"
          ( A.info (parserBlockquoteWithOptionalPullQuote <**> A.helper)
          $ A.progDesc "Generate a pull quote component with no quote symbols"
          )

      -- Button
      <> A.command
          "button--primary"
          ( A.info (parserButtonPrimary <**> A.helper)
          $ A.progDesc "Generate a primary button component"
          )
      <> A.command
          "button--primary-large"
          ( A.info (parserButtonPrimaryLarge <**> A.helper)
          $ A.progDesc "Generate a large primary button component"
          )
      <> A.command
          "button--primary-disabled"
          ( A.info (parserButtonPrimaryDisabled <**> A.helper)
          $ A.progDesc "Generate a disabled primary button component"
          )
      <> A.command
          "button--secondary"
          ( A.info (parserButtonSecondary <**> A.helper)
          $ A.progDesc "Generate a secondary button component"
          )
      <> A.command
          "button--secondary-large"
          ( A.info (parserButtonSecondaryLarge <**> A.helper)
          $ A.progDesc "Generate a large secondary button component"
          )
      <> A.command
          "button--secondary-disabled"
          ( A.info (parserButtonSecondaryDisabled <**> A.helper)
          $ A.progDesc "Generate a disabled secondary button component"
          )
      <> A.command
          "button--full-width"
          ( A.info (parserButtonFullWidth <**> A.helper)
          $ A.progDesc "Generate a full-width button component"
          )

      -- Buttonlink
      <> A.command
          "buttonlink--primary"
          ( A.info (parserButtonLinkPrimary <**> A.helper)
          $ A.progDesc "Generate a primary button component"
          )
      <> A.command
          "buttonlink--primary-large"
          ( A.info (parserButtonLinkPrimaryLarge <**> A.helper)
          $ A.progDesc "Generate a large primary button component"
          )
      <> A.command
          "buttonlink--primary-disabled"
          ( A.info (parserButtonLinkPrimaryDisabled <**> A.helper)
          $ A.progDesc "Generate a disabled primary button component"
          )
      <> A.command
          "buttonlink--secondary"
          ( A.info (parserButtonLinkSecondary <**> A.helper)
          $ A.progDesc "Generate a secondary button component"
          )
      <> A.command
          "buttonlink--secondary-large"
          ( A.info (parserButtonLinkSecondaryLarge <**> A.helper)
          $ A.progDesc "Generate a large secondary button component"
          )
      <> A.command
          "buttonlink--secondary-disabled"
          ( A.info (parserButtonLinkSecondaryDisabled <**> A.helper)
          $ A.progDesc "Generate a disabled secondary button component"
          )
      <> A.command
          "buttonlink--full-width"
          ( A.info (parserButtonLinkFullWidth <**> A.helper)
          $ A.progDesc "Generate a full-width button component"
          )

      -- Checkbox
      <> A.command
          "checkbox--default"
          ( A.info (parserCheckboxDefault <**> A.helper)
          $ A.progDesc "Generate a checkbox component"
          )
      <> A.command
          "checkbox--pill"
          ( A.info (parserCheckboxPill <**> A.helper)
          $ A.progDesc "Generate a checkbox pill component"
          )

      -- Codeblock
      <> A.command
          "codeblock--default"
          ( A.info (parserCodeblock <**> A.helper)
          $ A.progDesc "Generate a codeblock component"
          )
      <> A.command
          "codeblock--with-table"
          ( A.info (parserCodeblockWithTable <**> A.helper)
          $ A.progDesc "Generate a codeblock example with a table"
          )
      <> A.command
          "codeblock--editable"
          ( A.info (parserCodeblockEditable <**> A.helper)
          $ A.progDesc "Generate a content-editable codeblock component"
          )
      <> A.command
          "codeblock--textarea"
          ( A.info (parserCodeblockTextArea <**> A.helper)
          $ A.progDesc "Generate a textarea codeblock component"
          )
      <> A.command
          "codeblock--editable-bottom-button"
          ( A.info (parserCodeblockEditableBottomButton <**> A.helper)
          $ A.progDesc "Generate a content-editable codeblock component, \
              \with a button at the bottom"
          )
      <> A.command
          "codeblock--editable-toolbar-button"
          ( A.info (parserCodeblockEditableToolbarButton <**> A.helper)
          $ A.progDesc "Generate a content-editable codeblock component, \
              \with a button in the toolbar"
          )
      <> A.command
          "codeblock--textarea-bottom-button"
          ( A.info (parserCodeblockTextAreaBottomButton <**> A.helper)
          $ A.progDesc "Generate a textarea codeblock component, \
              \with a button at the bottom"
          )
      <> A.command
          "codeblock--textarea-toolbar-button"
          ( A.info (parserCodeblockTextAreaToolbarButton <**> A.helper)
          $ A.progDesc "Generate a textarea codeblock component, \
              \with a button in the toolbar"
          )

      -- Color
      <> A.command
          "colour--text"
          ( A.info (parserColorText <**> A.helper)
          $ A.progDesc "Generate a text colour palette"
          )
      <> A.command
          "colour--background"
          ( A.info (parserColorBackground <**> A.helper)
          $ A.progDesc "Generate a background colour palette"
          )
      <> A.command
          "colour--samples"
          ( A.info (parserColorSamples <**> A.helper)
          $ A.progDesc "Generate a colour cards"
          )

      -- Footer
      <> A.command
          "footer"
          ( A.info (parserFooter <**> A.helper)
          $ A.progDesc "Generate a footer component"
          )

      -- Form
      <> A.command
          "form--login"
          ( A.info (parserFormLogin <**> A.helper)
          $ A.progDesc "Generate a login form example"
          )

      -- Layout
      <> A.command
          "layout--default"
          ( A.info (parserLayoutDefault <**> A.helper)
          $ A.progDesc "Generate the default layout"
          )

      -- Navigation
      <> A.command
          "nav"
          ( A.info (parserNav <**> A.helper)
          $ A.progDesc "Generate a nav component"
          )
      <> A.command
          "navigation--navigation"
          ( A.info (parserNavigation <**> A.helper)
          $ A.progDesc "Generate a navigation bar example"
          )
      <> A.command
          "navigation--navigation-space-between"
          ( A.info (parserNavigationSpaceBetween <**> A.helper)
          $ A.progDesc "Generate a navigation bar example"
          )

      -- Radio
      <> A.command
          "radio--pill"
          ( A.info (parserRadioDefault <**> A.helper)
          $ A.progDesc "Generate vertical pill radio components"
          )
      <> A.command
          "radio--pill-inline"
          ( A.info (parserRadioPillInline <**> A.helper)
          $ A.progDesc "Generate horizontal pill radio components"
          )
      <> A.command
          "radio--checkbox"
          ( A.info (parserRadioCheckbox <**> A.helper)
          $ A.progDesc "Generate vertical box radio components"
          )
      <> A.command
          "radio--checkbox-inline"
          ( A.info (parserRadioCheckboxInline <**> A.helper)
          $ A.progDesc "Generate horizontal box radio components"
          )

      -- Side panel
      <> A.command
          "sidepanel--default"
          ( A.info (parserSidePanel <**> A.helper)
          $ A.progDesc "Generate a side panel component"
          )
      <> A.command
          "sidepanel--usage"
          ( A.info (parserSidePanelUsage <**> A.helper)
          $ A.progDesc "Generate a page showin a side panel in context"
          )

      -- Sidebar
      <> A.command
          "sidebar--default"
          ( A.info (parserSidebar <**> A.helper)
          $ A.progDesc "Generate a sidebar component"
          )
      <> A.command
          "sidebar--usage"
          ( A.info (parserSidebarUsage <**> A.helper)
          $ A.progDesc "Generate a page showing a sidebar in context"
          )

      -- Status code
      <> A.command
          "status-code--error-400"
          ( A.info (parserStatusCodeError400 <**> A.helper)
          $ A.progDesc "Generate a 400 error component"
          )
      <> A.command
          "status-code--error-404"
          ( A.info (parserStatusCodeError404 <**> A.helper)
          $ A.progDesc "Generate a 404 error component"
          )

      -- Table
      <> A.command
          "table--default"
          ( A.info (parserTableDefault <**> A.helper)
          $ A.progDesc "Generate a table example"
          )
      <> A.command
          "table--compact"
          ( A.info (parserTableCompact <**> A.helper)
          $ A.progDesc "Generate a compact table example"
          )
      <> A.command
          "table--with-column-divider"
          ( A.info (parserTableWithColumnDivider <**> A.helper)
          $ A.progDesc "Generate a table example, with column divider"
          )
      <> A.command
          "table--with-column-divider-compact"
          ( A.info (parserTableWithColumnDividerCompact <**> A.helper)
          $ A.progDesc "Generate a compact table example, with column divider"
          )

      -- Title
      <> A.command
          "title--default"
          ( A.info (parserTitle <**> A.helper)
          $ A.progDesc "Generate a title"
          )
      <> A.command
          "title--subtitle"
          ( A.info (parserTitleSubtitle <**> A.helper)
          $ A.progDesc "Generate a subtitle"
          )
      <> A.command
          "title--jumbo"
          ( A.info (parserTitleJumbo <**> A.helper)
          $ A.progDesc "Generate a jumbo title"
          )
      <> A.command
          "title--subtitle-jumbo"
          ( A.info (parserTitleSubtitleJumbo <**> A.helper)
          $ A.progDesc "Generate a jumbo subtitle"
          )
      <> A.command
          "title--usage"
          ( A.info (parserTitleUsage <**> A.helper)
          $ A.progDesc "Generate a page combining title elements"
          )
      <> A.command
          "title--jumbo-usage"
          ( A.info (parserTitleJumboUsage <**> A.helper)
          $ A.progDesc "Generate a page combining jumbo elements"
          )

      -- Typography
      <> A.command
          "typography--heading-1"
          ( A.info (parserTypographyHeading1 <**> A.helper)
          $ A.progDesc "Generate a heading of level 1"
          )
      <> A.command
          "typography--heading-2"
          ( A.info (parserTypographyHeading2 <**> A.helper)
          $ A.progDesc "Generate a heading of level 2"
          )
      <> A.command
          "typography--heading-3"
          ( A.info (parserTypographyHeading3 <**> A.helper)
          $ A.progDesc "Generate a heading of level 3"
          )
      <> A.command
          "typography--heading-4"
          ( A.info (parserTypographyHeading4 <**> A.helper)
          $ A.progDesc "Generate a heading of level 4"
          )
      <> A.command
          "typography--heading-5"
          ( A.info (parserTypographyHeading5 <**> A.helper)
          $ A.progDesc "Generate a heading of level 5"
          )
      <> A.command
          "typography--heading-6"
          ( A.info (parserTypographyHeading6 <**> A.helper)
          $ A.progDesc "Generate a heading of level 6"
          )
      <> A.command
          "typography--paragraph"
          ( A.info (parserTypographyParagraph <**> A.helper)
          $ A.progDesc "Generate a paragraph"
          )
      <> A.command
          "typography--usage"
          ( A.info (parserTypographyUsage <**> A.helper)
          $ A.progDesc "Generate a page with variations of typography elements"
          )

      -- Whitespace
      <> A.command
          "whitespace--auto-width"
          ( A.info (parserWhitespaceAutoWidth <**> A.helper)
          $ A.progDesc "Generate a whitespace component with auto-width"
          )
      <> A.command
          "whitespace--negative-margins"
          ( A.info (parserWhitespaceNegativeMargins <**> A.helper)
          $ A.progDesc "Generate a negative margins whitespace component"
          )
      <> A.command
          "whitespace--full-width"
          ( A.info (parserWhitespaceFullWidth <**> A.helper)
          $ A.progDesc "Generate a full-width whitespace component"
          )
      <> A.command
          "whitespace--examples"
          ( A.info (parserWhitespaceExamples <**> A.helper)
          $ A.progDesc
              "Generate a page with variations of whitespace components"
          )
      )


--------------------------------------------------------------------------------
parserGenerateGuide :: A.Parser Command
parserGenerateGuide = pure GenerateGuide

parserGenerateTemplates :: A.Parser Command
parserGenerateTemplates = GenerateTemplates <$> A.switch
  (A.long "docs" <> A.help "Use a prefix suitable for GitHub Pages.")

parserWrapper :: A.Parser Command
parserWrapper = pure Wrapper


--------------------------------------------------------------------------------
parserAnchorBlue :: A.Parser Command
parserAnchorBlue = pure GenerateAnchorBlue

parserAnchorBlack :: A.Parser Command
parserAnchorBlack = pure GenerateAnchorBlack

parserNav :: A.Parser Command
parserNav = pure GenerateNav

parserBannerGreen :: A.Parser Command
parserBannerGreen = pure GenerateBannerGreen

parserBannerRed :: A.Parser Command
parserBannerRed = pure GenerateBannerRed

parserBannerYellow :: A.Parser Command
parserBannerYellow = pure GenerateBannerYellow

parserBlockquoteDefault :: A.Parser Command
parserBlockquoteDefault = pure GenerateBlockquoteDefault

parserBlockquotePullQuote :: A.Parser Command
parserBlockquotePullQuote = pure GenerateBlockquotePullQuote

parserBlockquoteWithOptionalPullQuote :: A.Parser Command
parserBlockquoteWithOptionalPullQuote =
  pure GenerateBlockquoteWithOptionalPullQuote

parserButtonPrimary :: A.Parser Command
parserButtonPrimary = pure GenerateButtonPrimary

parserButtonPrimaryLarge :: A.Parser Command
parserButtonPrimaryLarge = pure GenerateButtonPrimaryLarge

parserButtonPrimaryDisabled :: A.Parser Command
parserButtonPrimaryDisabled = pure GenerateButtonPrimaryDisabled

parserButtonSecondary :: A.Parser Command
parserButtonSecondary = pure GenerateButtonSecondary

parserButtonSecondaryLarge :: A.Parser Command
parserButtonSecondaryLarge = pure GenerateButtonSecondaryLarge

parserButtonSecondaryDisabled :: A.Parser Command
parserButtonSecondaryDisabled = pure GenerateButtonSecondaryDisabled

parserButtonFullWidth :: A.Parser Command
parserButtonFullWidth = pure GenerateButtonFullWidth

parserButtonLinkPrimary :: A.Parser Command
parserButtonLinkPrimary = pure GenerateButtonLinkPrimary

parserButtonLinkPrimaryLarge :: A.Parser Command
parserButtonLinkPrimaryLarge = pure GenerateButtonLinkPrimaryLarge

parserButtonLinkPrimaryDisabled :: A.Parser Command
parserButtonLinkPrimaryDisabled = pure GenerateButtonLinkPrimaryDisabled

parserButtonLinkSecondary :: A.Parser Command
parserButtonLinkSecondary = pure GenerateButtonLinkSecondary

parserButtonLinkSecondaryLarge :: A.Parser Command
parserButtonLinkSecondaryLarge = pure GenerateButtonLinkSecondaryLarge

parserButtonLinkSecondaryDisabled :: A.Parser Command
parserButtonLinkSecondaryDisabled = pure GenerateButtonLinkSecondaryDisabled

parserButtonLinkFullWidth :: A.Parser Command
parserButtonLinkFullWidth = pure GenerateButtonLinkFullWidth

parserCheckboxDefault :: A.Parser Command
parserCheckboxDefault = pure GenerateCheckboxDefault

parserCheckboxPill :: A.Parser Command
parserCheckboxPill = pure GenerateCheckboxPill

parserCodeblock :: A.Parser Command
parserCodeblock = pure GenerateCodeblock

parserCodeblockWithTable :: A.Parser Command
parserCodeblockWithTable = pure GenerateCodeblockWithTable

parserCodeblockEditable :: A.Parser Command
parserCodeblockEditable = pure GenerateCodeblockEditable

parserCodeblockTextArea :: A.Parser Command
parserCodeblockTextArea = pure GenerateCodeblockTextArea

parserCodeblockEditableBottomButton :: A.Parser Command
parserCodeblockEditableBottomButton =
  pure GenerateCodeblockEditableBottomButton

parserCodeblockEditableToolbarButton :: A.Parser Command
parserCodeblockEditableToolbarButton =
  pure GenerateCodeblockEditableToolbarButton

parserCodeblockTextAreaBottomButton :: A.Parser Command
parserCodeblockTextAreaBottomButton =
  pure GenerateCodeblockTextAreaBottomButton

parserCodeblockTextAreaToolbarButton :: A.Parser Command
parserCodeblockTextAreaToolbarButton =
  pure GenerateCodeblockTextAreaToolbarButton

parserColorText :: A.Parser Command
parserColorText = pure GenerateColorText

parserColorBackground :: A.Parser Command
parserColorBackground = pure GenerateColorBackground

parserColorSamples :: A.Parser Command
parserColorSamples = pure GenerateColorSamples

parserFooter :: A.Parser Command
parserFooter = pure GenerateFooter

parserRadioDefault :: A.Parser Command
parserRadioDefault = pure GenerateRadioDefault

parserRadioPillInline :: A.Parser Command
parserRadioPillInline = pure GenerateRadioPillInline

parserRadioCheckbox :: A.Parser Command
parserRadioCheckbox = pure GenerateRadioCheckbox

parserRadioCheckboxInline :: A.Parser Command
parserRadioCheckboxInline = pure GenerateRadioCheckboxInline

parserSidePanel :: A.Parser Command
parserSidePanel = pure GenerateSidePanel

parserSidePanelUsage :: A.Parser Command
parserSidePanelUsage = pure GenerateSidePanelUsage

parserSidebar :: A.Parser Command
parserSidebar = pure GenerateSidebar

parserSidebarUsage :: A.Parser Command
parserSidebarUsage = pure GenerateSidebarUsage

parserStatusCodeError400 :: A.Parser Command
parserStatusCodeError400 = pure GenerateStatusCodeError400

parserStatusCodeError404 :: A.Parser Command
parserStatusCodeError404 = pure GenerateStatusCodeError404

parserTableDefault :: A.Parser Command
parserTableDefault = pure GenerateTableDefault

parserTableCompact :: A.Parser Command
parserTableCompact = pure GenerateTableCompact

parserTableWithColumnDivider :: A.Parser Command
parserTableWithColumnDivider = pure GenerateTableWithColumnDivider

parserTableWithColumnDividerCompact :: A.Parser Command
parserTableWithColumnDividerCompact =
  pure GenerateTableWithColumnDividerCompact

parserTitleJumbo :: A.Parser Command
parserTitleJumbo = pure GenerateTitleJumbo

parserTitleSubtitleJumbo :: A.Parser Command
parserTitleSubtitleJumbo = pure GenerateTitleSubtitleJumbo

parserTitle :: A.Parser Command
parserTitle = pure GenerateTitle

parserTitleSubtitle :: A.Parser Command
parserTitleSubtitle = pure GenerateTitleSubtitle

parserTitleJumboUsage :: A.Parser Command
parserTitleJumboUsage = pure GenerateTitleJumboUsage

parserTitleUsage :: A.Parser Command
parserTitleUsage = pure GenerateTitleUsage

parserTypographyHeading1 :: A.Parser Command
parserTypographyHeading1 = pure GenerateTypographyHeading1

parserTypographyHeading2 :: A.Parser Command
parserTypographyHeading2 = pure GenerateTypographyHeading2

parserTypographyHeading3 :: A.Parser Command
parserTypographyHeading3 = pure GenerateTypographyHeading3

parserTypographyHeading4 :: A.Parser Command
parserTypographyHeading4 = pure GenerateTypographyHeading4

parserTypographyHeading5 :: A.Parser Command
parserTypographyHeading5 = pure GenerateTypographyHeading5

parserTypographyHeading6 :: A.Parser Command
parserTypographyHeading6 = pure GenerateTypographyHeading6

parserTypographyParagraph :: A.Parser Command
parserTypographyParagraph = pure GenerateTypographyParagraph

parserTypographyUsage :: A.Parser Command
parserTypographyUsage = pure GenerateTypographyUsage

parserWhitespaceAutoWidth :: A.Parser Command
parserWhitespaceAutoWidth = pure GenerateWhitespaceAutoWidth

parserWhitespaceNegativeMargins :: A.Parser Command
parserWhitespaceNegativeMargins = pure GenerateWhitespaceNegativeMargins

parserWhitespaceFullWidth :: A.Parser Command
parserWhitespaceFullWidth = pure GenerateWhitespaceFullWidth

parserWhitespaceExamples :: A.Parser Command
parserWhitespaceExamples = pure GenerateWhitespaceExamples

parserFormLogin :: A.Parser Command
parserFormLogin = pure GenerateFormLogin

parserNavigation :: A.Parser Command
parserNavigation = pure GenerateNavigation

parserNavigationSpaceBetween :: A.Parser Command
parserNavigationSpaceBetween = pure GenerateNavigationSpaceBetween

parserLayoutDefault :: A.Parser Command
parserLayoutDefault = pure GenerateLayoutDefault

parserListCategories :: A.Parser Command
parserListCategories = pure ListCategories

parserListStories :: A.Parser Command
parserListStories = pure ListStories

parserJsImportStories :: A.Parser Command
parserJsImportStories = pure JsImportStories

parserJsStories :: A.Parser Command
parserJsStories = pure JsStories
