module Hypered.Design.Examples where

import Hypered.Html.Tachyons
  ( footer

  -- For individual componenents
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
  , hrDefault, hrDivider
  , imageDefault, imageNegativePull, imageFullWidth, imageWithCaption
  , layoutDefault, layoutBlogList, layoutBlogPost1, layoutBlogPost2
  , layoutWithSidebar
  , modalTextContent, modalButtonLabel, modalTextLabel
  , navigationBlockDefault, navigationBlockUsage
  , navigationDefault, navigationSpaceBetween
  , tableDefault, tableCompact, tableWithColumnDivider
  , tableWithColumnDividerCompact
  )
import           Protolude
import           Text.Blaze.Html5               ( Html )


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


--------------------------------------------------------------------------------
hrExample :: Html
hrExample = hrDefault

hrDividerExample :: Html
hrDividerExample = hrDivider


--------------------------------------------------------------------------------
imageExample :: Html
imageExample = imageDefault

imageNegativePullExample :: Html
imageNegativePullExample = imageNegativePull

imageFullWidthExample :: Html
imageFullWidthExample = imageFullWidth

imageWithCaptionExample :: Html
imageWithCaptionExample = imageWithCaption

-- imageExamples :: Html
-- imageExamples = imageExamples


--------------------------------------------------------------------------------
-- inputTextExample :: Html
-- inputTextExample =

-- inputPasswordExample :: Html
-- inputPasswordExample =

-- inputNumberExample :: Html
-- inputNumberExample =

-- inputWithMessageExample :: Html
-- inputWithMessageExample =

-- inputUsage :: Html
-- inputUsage =


------------------------------------------------------------------------------
layoutExample :: Html
layoutExample = layoutDefault

layoutBlogListExample :: Html
layoutBlogListExample = layoutBlogList

layoutBlogPost1Example :: Html
layoutBlogPost1Example = layoutBlogPost1

layoutBlogPost2Example :: Html
layoutBlogPost2Example = layoutBlogPost2

layoutWithSidebarExample :: Html
layoutWithSidebarExample = layoutWithSidebar


--------------------------------------------------------------------------------
-- listOrderedExample :: Html
-- listOrderedExample =

-- listUnorderedExample :: Html
-- listUnorderedExample =


--------------------------------------------------------------------------------
modalTextContentExample :: Html
modalTextContentExample = modalTextContent

modalButtonLabelExample :: Html
modalButtonLabelExample = modalButtonLabel

modalTextLabelExample :: Html
modalTextLabelExample = modalTextLabel


--------------------------------------------------------------------------------
navigationBlockExample :: Html
navigationBlockExample = navigationBlockDefault

navigationBlockUsageExample :: Html
navigationBlockUsageExample = navigationBlockUsage


--------------------------------------------------------------------------------
navigationExample :: Html
navigationExample = navigationDefault

navigationSpaceBetweenExample :: Html
navigationSpaceBetweenExample = navigationSpaceBetween


--------------------------------------------------------------------------------
tableDefaultExample :: Html
tableDefaultExample = tableDefault -- TODO Arguments.

tableCompactExample :: Html
tableCompactExample = tableCompact

tableWithColumnDividerExample :: Html
tableWithColumnDividerExample = tableWithColumnDivider

tableWithColumnDividerCompactExample :: Html
tableWithColumnDividerCompactExample = tableWithColumnDividerCompact
