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
    GenerateTemplates Bool
    -- ^ If True, use a prefix suitable for GitHub Pages (i.e. the /design
    -- prefix, since the static site is at hypered.github.io/design).
  | GenerateGuide
  | Wrapper
    -- ^ The document wrapper. This should match `pages/_app.js`.

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
  | GenerateCodeblockWithTableExample

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
  | GenerateInputExample

  | GenerateLayout
  | GenerateLayoutBlogList
  | GenerateLayoutBlogPost
  | GenerateLayoutWithSidebar

  | GenerateListOrdered
  | GenerateListUnordered

  -- TODO Modal.

  | GenerateNavigationBlock
  | GenerateNavigationBlockUsageExample

  | GenerateNav
  | GenerateNavigation
  | GenerateNavigationSpaceBetween

  | GenerateRadioDefault
  | GenerateRadioPillInline
  | GenerateRadioCheckbox
  | GenerateRadioCheckboxInline

  | GenerateSidePanel
  | GenerateSidePanelUsageExample

  | GenerateSidebar
  | GenerateSidebarUsageExample

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
  | GenerateTitleJumboUsageExample
  | GenerateTitleUsageExample

  | GenerateTypographyHeading1
  | GenerateTypographyHeading2
  | GenerateTypographyHeading3
  | GenerateTypographyHeading4
  | GenerateTypographyHeading5
  | GenerateTypographyHeading6
  | GenerateTypographyParagraph
  | GenerateTypographyUsageExample

  | GenerateWhitespaceAutoWidth
  | GenerateWhitespaceNegativeMargins
  | GenerateWhitespaceFullWidth
  | GenerateWhitespaceExamples

    -- Stories from Storybook

  | GenerateLayoutDefault

    -- Helpers to explore Storybook stories

  | ListCategories
  | ListStories
  | JsImportStories
  | JsStories
  deriving (Eq, Show)


--------------------------------------------------------------------------------
parserInfo :: A.ParserInfo Command
parserInfo =
  A.info (parser <**> A.helper)
    $  A.fullDesc
    <> A.header "hypered-design - The source of truth for the Hypered design system"
    <> A.progDesc
         "The Hypered design system."


--------------------------------------------------------------------------------
parser :: A.Parser Command
parser =
  A.subparser
      (  A.command
          "generate-templates"
          ( A.info (parserGenerateTemplates <**> A.helper)
          $ A.progDesc "Generate HTML templates that can be used with Pandoc"
          )

      <> A.command
          "generate-guide"
          ( A.info (parserGenerateGuide <**> A.helper)
          $ A.progDesc "A dummy command"
          )

      <> A.command
          "wrapper"
          ( A.info (parserWrapper <**> A.helper)
          $ A.progDesc "Generate the document wrapper. This should match `pages/_app.js`."
          )

      <> A.command
          "a--blue"
          ( A.info (parserAnchorBlue <**> A.helper)
          $ A.progDesc "Generate a blue link component."
          )

      <> A.command
          "a--black"
          ( A.info (parserAnchorBlack <**> A.helper)
          $ A.progDesc "Generate a black link component."
          )

      <> A.command
          "banner--green"
          ( A.info (parserBannerGreen <**> A.helper)
          $ A.progDesc "Generate a green banner component."
          )

      <> A.command
          "banner--red"
          ( A.info (parserBannerRed <**> A.helper)
          $ A.progDesc "Generate a red banner component."
          )

      <> A.command
          "banner--yellow"
          ( A.info (parserBannerYellow <**> A.helper)
          $ A.progDesc "Generate a yellow banner component."
          )

      <> A.command
          "blockquote--default"
          ( A.info (parserBlockquoteDefault <**> A.helper)
          $ A.progDesc "Generate a blockquote component."
          )

      <> A.command
          "blockquote--pull-quote"
          ( A.info (parserBlockquotePullQuote <**> A.helper)
          $ A.progDesc "Generate a pull quote component."
          )

      <> A.command
          "blockquote--with-optional-pull-quote"
          ( A.info (parserBlockquoteWithOptionalPullQuote <**> A.helper)
          $ A.progDesc "Generate a pull quote component with no quote symbols."
          )

      <> A.command
          "button--primary"
          ( A.info (parserButtonPrimary <**> A.helper)
          $ A.progDesc "Generate a primary button component."
          )

      <> A.command
          "button--primary-large"
          ( A.info (parserButtonPrimaryLarge <**> A.helper)
          $ A.progDesc "Generate a large primary button component."
          )

      <> A.command
          "button--primary-disabled"
          ( A.info (parserButtonPrimaryDisabled <**> A.helper)
          $ A.progDesc "Generate a disabled primary button component."
          )

      <> A.command
          "button--secondary"
          ( A.info (parserButtonSecondary <**> A.helper)
          $ A.progDesc "Generate a secondary button component."
          )

      <> A.command
          "button--secondary-large"
          ( A.info (parserButtonSecondaryLarge <**> A.helper)
          $ A.progDesc "Generate a secondary large button component."
          )

      <> A.command
          "button--secondary-disabled"
          ( A.info (parserButtonSecondaryDisabled <**> A.helper)
          $ A.progDesc "Generate a disabled secondary button component."
          )

      <> A.command
          "button--full-width"
          ( A.info (parserButtonFullWidth <**> A.helper)
          $ A.progDesc "Generate a full-width button component."
          )

      <> A.command
          "buttonlink--primary"
          ( A.info (parserButtonLinkPrimary <**> A.helper)
          $ A.progDesc "Generate a primary button component."
          )

      <> A.command
          "buttonlink--primary-large"
          ( A.info (parserButtonLinkPrimaryLarge <**> A.helper)
          $ A.progDesc "Generate a large primary button component."
          )

      <> A.command
          "buttonlink--primary-disabled"
          ( A.info (parserButtonLinkPrimaryDisabled <**> A.helper)
          $ A.progDesc "Generate a disabled primary button component."
          )

      <> A.command
          "buttonlink--secondary"
          ( A.info (parserButtonLinkSecondary <**> A.helper)
          $ A.progDesc "Generate a secondary button component."
          )

      <> A.command
          "buttonlink--secondary-large"
          ( A.info (parserButtonLinkSecondaryLarge <**> A.helper)
          $ A.progDesc "Generate a secondary large button component."
          )

      <> A.command
          "buttonlink--secondary-disabled"
          ( A.info (parserButtonLinkSecondaryDisabled <**> A.helper)
          $ A.progDesc "Generate a disabled secondary button component."
          )

      <> A.command
          "buttonlink--full-width"
          ( A.info (parserButtonLinkFullWidth <**> A.helper)
          $ A.progDesc "Generate a full-width button component."
          )

      <> A.command
          "checkbox--default"
          ( A.info (parserCheckboxDefault <**> A.helper)
          $ A.progDesc "Generate a checkbox component."
          )

      <> A.command
          "checkbox--pill"
          ( A.info (parserCheckboxPill <**> A.helper)
          $ A.progDesc "Generate a checkbox pill component."
          )

      <> A.command
          "codeblock--default"
          ( A.info (parserCodeblock <**> A.helper)
          $ A.progDesc "Generate a codeblock component."
          )

      <> A.command
          "codeblock--with-table"
          ( A.info (parserCodeblockWithTable <**> A.helper)
          $ A.progDesc "Generate a codeblock example with a table."
          )

      <> A.command
          "footer"
          ( A.info (parserFooter <**> A.helper)
          $ A.progDesc "Generate a footer component."
          )

      <> A.command
          "radio--pill"
          ( A.info (parserRadioDefault <**> A.helper)
          $ A.progDesc "Generate a component."
          )
      <> A.command
          "radio--pill-inline"
          ( A.info (parserRadioPillInline <**> A.helper)
          $ A.progDesc "Generate a component."
          )
      <> A.command
          "radio--checkbox"
          ( A.info (parserRadioCheckbox <**> A.helper)
          $ A.progDesc "Generate a component."
          )
      <> A.command
          "radio--checkbox-inline"
          ( A.info (parserRadioCheckboxInline <**> A.helper)
          $ A.progDesc "Generate a component."
          )

      <> A.command
          "sidepanel--default"
          ( A.info (parserSidePanel <**> A.helper)
          $ A.progDesc "Generate a component."
          )
      <> A.command
          "sidepanel--usage"
          ( A.info (parserSidePanelUsageExample <**> A.helper)
          $ A.progDesc "Generate a component."
          )

      <> A.command
          "sidebar--default"
          ( A.info (parserSidebar <**> A.helper)
          $ A.progDesc "Generate a component."
          )
      <> A.command
          "sidebar--usage"
          ( A.info (parserSidebarUsageExample <**> A.helper)
          $ A.progDesc "Generate a component."
          )

      <> A.command
          "status-code--error-400"
          ( A.info (parserStatusCodeError400 <**> A.helper)
          $ A.progDesc "Generate a component."
          )
      <> A.command
          "status-code--error-404"
          ( A.info (parserStatusCodeError404 <**> A.helper)
          $ A.progDesc "Generate a component."
          )

      <> A.command
          "table--default"
          ( A.info (parserTableDefault <**> A.helper)
          $ A.progDesc "Generate a table example."
          )

      <> A.command
          "table--compact"
          ( A.info (parserTableCompact <**> A.helper)
          $ A.progDesc "Generate a compact table example."
          )

      <> A.command
          "table--with-column-divider"
          ( A.info (parserTableWithColumnDivider <**> A.helper)
          $ A.progDesc "Generate a table example, with column divider."
          )

      <> A.command
          "table--with-column-divider-compact"
          ( A.info (parserTableWithColumnDividerCompact <**> A.helper)
          $ A.progDesc "Generate a compact table example, with column divider."
          )

      <> A.command
          "title--jumbo"
          ( A.info (parserTitleJumbo <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "title--subtitle-jumbo"
          ( A.info (parserTitleSubtitleJumbo <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "title--title"
          ( A.info (parserTitle <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "title--subtitle"
          ( A.info (parserTitleSubtitle <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "title--jumbo-usage"
          ( A.info (parserTitleJumboUsageExample <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "title--title-usage"
          ( A.info (parserTitleUsageExample <**> A.helper)
          $ A.progDesc "Generate a example."
          )

      <> A.command
          "typography--heading-1"
          ( A.info (parserTypographyHeading1 <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "typography--heading-2"
          ( A.info (parserTypographyHeading2 <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "typography--heading-3"
          ( A.info (parserTypographyHeading3 <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "typography--heading-4"
          ( A.info (parserTypographyHeading4 <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "typography--heading-5"
          ( A.info (parserTypographyHeading5 <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "typography--heading-6"
          ( A.info (parserTypographyHeading6 <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "typography--paragraph"
          ( A.info (parserTypographyParagraph <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "typography--usage"
          ( A.info (parserTypographyUsageExample <**> A.helper)
          $ A.progDesc "Generate a example."
          )

      <> A.command
          "whitespace--auto-width"
          ( A.info (parserWhitespaceAutoWidth <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "whitespace--negative-margins"
          ( A.info (parserWhitespaceNegativeMargins <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "whitespace--full-width"
          ( A.info (parserWhitespaceFullWidth <**> A.helper)
          $ A.progDesc "Generate a example."
          )
      <> A.command
          "whitespace--examples"
          ( A.info (parserWhitespaceExamples <**> A.helper)
          $ A.progDesc "Generate a example."
          )

      <> A.command
          "form--login"
          ( A.info (parserFormLogin <**> A.helper)
          $ A.progDesc "Generate a login form example."
          )

      <> A.command
          "nav"
          ( A.info (parserNav <**> A.helper)
          $ A.progDesc "Generate a nav component."
          )

      <> A.command
          "navigation--navigation"
          ( A.info (parserNavigation <**> A.helper)
          $ A.progDesc "Generate a navigation bar example."
          )

      <> A.command
          "navigation--navigation-space-between"
          ( A.info (parserNavigationSpaceBetween <**> A.helper)
          $ A.progDesc "Generate a navigation bar example."
          )

      <> A.command
          "layout--default"
          ( A.info (parserLayoutDefault <**> A.helper)
          $ A.progDesc "Generate the default layout."
          )

      <> A.command
          "list-categories"
          ( A.info (parserListCategories <**> A.helper)
          $ A.progDesc "List Storybook categories."
          )

      <> A.command
          "list-stories"
          ( A.info (parserListStories <**> A.helper)
          $ A.progDesc "List Storybook stories."
          )

      <> A.command
          "js-import-stories"
          ( A.info (parserJsImportStories <**> A.helper)
          $ A.progDesc "Generate JS code to import the Storybook stories."
          )

      <> A.command
          "js-stories"
          ( A.info (parserJsStories <**> A.helper)
          $ A.progDesc "Generate JS code to render Storybook stories."
          )
      )


--------------------------------------------------------------------------------
parserGenerateTemplates :: A.Parser Command
parserGenerateTemplates = GenerateTemplates <$> A.switch
  (A.long "docs" <> A.help "Use a prefix suitable for GitHub Pages.")

parserGenerateGuide :: A.Parser Command
parserGenerateGuide = pure GenerateGuide

parserWrapper :: A.Parser Command
parserWrapper = pure Wrapper

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
parserCodeblockWithTable = pure GenerateCodeblockWithTableExample

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

parserSidePanelUsageExample :: A.Parser Command
parserSidePanelUsageExample = pure GenerateSidePanelUsageExample

parserSidebar :: A.Parser Command
parserSidebar = pure GenerateSidebar

parserSidebarUsageExample :: A.Parser Command
parserSidebarUsageExample = pure GenerateSidebarUsageExample

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

parserTitleJumboUsageExample :: A.Parser Command
parserTitleJumboUsageExample = pure GenerateTitleJumboUsageExample

parserTitleUsageExample :: A.Parser Command
parserTitleUsageExample = pure GenerateTitleUsageExample

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

parserTypographyUsageExample :: A.Parser Command
parserTypographyUsageExample = pure GenerateTypographyUsageExample

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
