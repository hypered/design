module Hypered.Design.Guide
  ( generateGuide
  ) where

import           Hypered.Design.Examples
import           Hypered.Design.IO
  ( generate, generate' )
import           Hypered.Html.Tachyons
  ( Font(Inter)
  , Config(..)
  , defaultConfig
  , loginForm, loginFormReesd
  , imageExamples
  , inputTextExample, inputPasswordExample, inputNumberExample
  , inputWithMessageExample, inputUsage
  , listOrderedExample, listUnorderedExample
  , exampleLoginForm, exampleLoginFormReesd, exampleRegisterForm, exampleResetForm
  , exampleResetFormReesd
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
import           Text.Blaze.Html5               ( (!), Html )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A


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
        H.li $ H.a ! A.href "example--login-form.html" $
          "Example, login form"
        H.li $ H.a ! A.href "example--login-form-reesd.html" $
          "Example, login form (Reesd)"
        H.li $ H.a ! A.href "example--register-form.html" $
          "Example, register form"
        H.li $ H.a ! A.href "example--reset-form.html" $
          "Example, reset form"
        H.li $ H.a ! A.href "example--reset-form-reesd.html" $
          "Example, reset form (Reesd)"
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

  -- Components

  mapM_
    (\(cat, variants) ->
      mapM_
        (\(_, href, f) ->
          generate href ("Hypered style guide - " <> cat) f)
        variants)
    guideData

  -- Example usage

  generate' "example--login-form.html"
    "Hypered style guide - Login form example"
    conf (const exampleLoginForm)
  generate' "example--login-form-reesd.html"
    "Hypered style guide - Login form example (Reesd)"
    conf (const exampleLoginFormReesd)
  generate' "example--register-form.html"
    "Hypered style guide - Register form example"
    conf (const exampleRegisterForm)
  generate' "example--reset-form.html"
    "Hypered style guide - Reset form example"
    conf (const exampleResetForm)
  generate' "example--reset-form-reesd.html"
    "Hypered style guide - Reset form example (Reesd)"
    conf (const exampleResetFormReesd)
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
