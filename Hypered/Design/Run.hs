{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypered.Design.Run
  ( run
  ) where

import           Data.List (nub, tail)
import qualified Data.Text                     as T
import qualified Hypered.Design.Command        as Command
import           Hypered.Design.Examples
import           Hypered.Design.Guide           ( generateGuide )
import qualified Hypered.Design.Server         as Server
import           Hypered.Design.Stories         ( stories )
import           Hypered.Design.Templates       ( generateTemplates )
import           Hypered.Html
  ( defaultConfig, document

  , loginForm
  , imageExamples
  , inputTextExample, inputPasswordExample, inputNumberExample
  , inputWithMessageExample, inputUsage
  , listOrderedExample, listUnorderedExample
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
import           Text.Blaze.Html5 ((!))
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
