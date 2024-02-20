module Hypered.Html.Struct.Prototypes.Refli.LandingPage where

import qualified Data.Text.Lazy as TL
import Hypered.Html.Helpers
import Hypered.Html.Struct.Prototypes.Refli.Common as Struct
import Protolude hiding (div)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Svg11            as S
import qualified Text.Blaze.Svg11.Attributes as SA

--------------------------------------------------------------------------------
data LandingPageTexts = LandingPageTexts
  { landingPageLanguage :: Text
  , landingPageTitle :: Text
  , landingPageDescription :: Text
  , landingPageParagraph1 :: Text
  , landingPageParagraph2 :: Text
  }

data LandingPageCaptureFormTexts = LandingPageCaptureFormTexts
  { landingPageCaptureFormLanguage :: Text
  , landingPageCaptureFormTitle :: Text
  , landingPageCaptureFormFieldLabel :: Text
  , landingPageCaptureFormSubmit :: Text
  , landingPageCaptureFormPrivacyNotice :: Text
  }

data FooFormTexts = FooFormTexts
  { fooFormLanguage :: Text
  , fooFormTitle :: Text
  , fooFormFieldLabel :: Text
  , fooFormSubmit :: Text
  }

data MessageFooSuccessTexts = MessageFooSuccessTexts
  { messageFooSuccessLanguage :: Text
  , messageFooSuccessTitle :: Text
  , messageFooSuccessParagraph1 :: Text
  }

data MessageSubscribeSuccessTexts = MessageSubscribeSuccessTexts
  { messageSubscribeSuccessLanguage :: Text
  , messageSubscribeSuccessTitle :: Text
  , messageSubscribeSuccessParagraph1 :: Text
  , messageSubscribeSuccessParagraph2 :: Text
  , messageSubscribeSuccessParagraph3 :: Text
  }

data LoginFormTexts = LoginFormTexts
  { loginFormLanguage :: Text
  , loginFormTitle :: Text
  , loginFormFieldLabel1 :: Text
  , loginFormFieldLabel2 :: Text
  , loginFormLink1 :: Text
  , loginFormLink2 :: Text
  , loginFormSubmit :: Text
  }

data DescribeFormPageTexts = DescribeFormPageTexts
  { describeFormPageLanguage :: Text
  , describeFormPageTitle :: Text
  , describeFormPageDescription :: Text
  , describeFormPageFormTitle :: Text
  , describeFormPageFormFieldLabel :: Text
  , describeFormPageFormSubmit :: Text
  }

-- Move elsewhere.
prototypeRefliRootPage :: Bool -> MainHeaderTexts -> LandingPageTexts -> NavigationBlockTexts -> Html
prototypeRefliRootPage autoreload mhTexts@MainHeaderTexts {..} LandingPageTexts {..} nbTexts = do
  refliDocument
    autoreload mainHeaderLanguage "Refli" landingPageDescription $
      prototypeRefliPage
        mainHeaderLanguage
        ""
        (prototypeRefliMainHeader mhTexts)
        nbTexts $ do
          -- TODO body.u-container-vertical.cover
          div "flow-all limit-42em" $ do
            H.p $ do
              "Refli is available in "
              H.a ! A.href "/en" $ "English"
              "."
            H.p $ do
              "Refli est disponible en "
              H.a ! A.href "/fr" $ "Français"
              "."
            H.p $ do
              "Refli is beschikbaar in het "
              H.a ! A.href "/nl" $ "Nederlands"
              "."

-- Move elsewhere.
prototypeRefliDescribeFormPage :: Bool -> Text -> MainHeaderTexts -> DescribeFormPageTexts  -> NavigationBlockTexts -> Html
prototypeRefliDescribeFormPage autoreload url mhTexts@MainHeaderTexts {..} DescribeFormPageTexts {..} nbTexts = do
  refliDocument
    autoreload mainHeaderLanguage describeFormPageTitle describeFormPageDescription $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts)
        nbTexts $ do
          div "switcher switch-at-60rem u-flow-c-4 u-space-after-c-4" $ do
            div "flow" $ do
              H.h1 $ H.text describeFormPageTitle
              H.p $ H.text describeFormPageDescription
            div "box" $
              H.form ! A.class_ "c-text flow"
                     ! A.method "POST"
                     ! A.action "/a/describe" $ do
                H.h4 $ H.text describeFormPageFormTitle
                H.div $ do
                  H.label ! A.for "monthly-gross-salary" $
                    H.text describeFormPageFormFieldLabel
                  H.input ! A.class_ "c-input"
                          ! A.name "monthly-gross-salary"
                          ! A.id "monthly-gross-salary"
                          ! A.type_ "text"
                          ! A.placeholder ""
                H.button ! A.class_ "c-button c-button--primary" ! A.type_ "submit" $ do
                  H.span $ H.text describeFormPageFormSubmit
                  arrowRight

prototypeRefliEchoPage :: Bool -> Text -> MainHeaderTexts -> NavigationBlockTexts -> TL.Text -> Html
prototypeRefliEchoPage autoreload url mhTexts@MainHeaderTexts {..} nbTexts content = do
  refliDocument
    autoreload mainHeaderLanguage "Echo" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts)
        nbTexts $ do
          standardSmallBox $ do
            H.h4 $ "Echo"
            H.pre $
              H.code $
                H.lazyText content

-- Similar to the echo page, but for Forming forms.
prototypeRefliSubmitPage :: Bool -> Text -> MainHeaderTexts -> NavigationBlockTexts -> Html -> Html
prototypeRefliSubmitPage autoreload url mhTexts@MainHeaderTexts {..} nbTexts content = do
  refliDocument
    autoreload mainHeaderLanguage "Echo" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts)
        nbTexts $
          standardSmallBox $ do
            H.h4 $ "Submittal"
            content

standardSmallBox :: Html -> Html
standardSmallBox content =
  div "max-48rem u-flow-c-4 u-space-after-c-4 center" $
    div "box u-flow-c-4" $
      div "c-text flow" $
        content

-- Use this "standard small" form function in other functions in this file.
standardSmallForm :: Text -> Html -> Html
standardSmallForm action content =
  div "max-48rem u-flow-c-4 u-space-after-c-4 center" $
    div "box u-flow-c-4" $
      H.form ! A.class_ "c-text flow"
             ! A.method "POST"
             ! A.action (H.toValue action) $
        content

-- Use this "base" page in the other functions in this file.
prototypeRefliBasePage :: Bool -> Text -> MainHeaderTexts -> NavigationBlockTexts -> Html -> Html
prototypeRefliBasePage autoreload url mhTexts@MainHeaderTexts {..} nbTexts content = do
  refliDocument
    autoreload mainHeaderLanguage "" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts)
        nbTexts $
          content

-- Move elsewhere.
prototypeRefliBlogIndexPage :: Bool -> Text -> MainHeaderTexts -> BlogPostPageTexts -> NavigationBlockTexts -> Html
prototypeRefliBlogIndexPage autoreload url mhTexts@MainHeaderTexts {..} BlogPostPageTexts {..} nbTexts = do
  refliDocument
    autoreload mainHeaderLanguage "Blog" "" $ -- TODO Description.
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts)
        nbTexts $ do
          div "c-content flow-all limit-42em" $
            H.h1 "Blog"

          div "c-content flow-all limit-42em" $ do
            H.h2 $ H.text blogPostPageTitle
            H.small ! A.class_ "breadcrumb" $ "2024-01-19"

          div "flow-all limit-42em" $ do
            H.p $ H.text blogPostPageDescription

            H.a ! A.class_ "c-button c-button--primary"
                ! A.href
                  (H.toValue $ "/" <> mainHeaderLanguage
                    <> "/blog/2024/01/19/introducing-refli") $ do
                H.span $ H.text blogPostReadMore
                arrowRight

arrowRight :: S.Svg
arrowRight =
  S.svg ! SA.viewbox "0 0 24 24" ! A.xmlns "http://www.w3.org/2000/svg" $
    S.path ! SA.d "M12.2929 5.29289C12.6834 4.90237 13.3166 4.90237 13.7071 5.29289L19.7071 11.2929C19.8946 11.4804 20 11.7348 20 12C20 12.2652 19.8946 12.5196 19.7071 12.7071L13.7071 18.7071C13.3166 19.0976 12.6834 19.0976 12.2929 18.7071C11.9024 18.3166 11.9024 17.6834 12.2929 17.2929L16.5858 13L5 13C4.44772 13 4 12.5523 4 12C4 11.4477 4.44772 11 5 11L16.5858 11L12.2929 6.70711C11.9024 6.31658 11.9024 5.68342 12.2929 5.29289Z" ! SA.fill "#595959"

-- Move elsewhere.
data BlogPostPageTexts = BlogPostPageTexts
  { blogPostPageLanguage :: Text
  , blogPostPageTitle :: Text
  , blogPostPageDescription :: Text
  , blogPostReadMore :: Text -- Use on the blog index, not the post.
  }

-- Move elsewhere.
prototypeRefliBlogPostPage :: Bool -> Text -> MainHeaderTexts -> BlogPostPageTexts -> Text -> NavigationBlockTexts -> Html
prototypeRefliBlogPostPage autoreload url mhTexts@MainHeaderTexts {..} BlogPostPageTexts {..} virtual nbTexts = do
  refliDocument
    autoreload blogPostPageLanguage blogPostPageTitle blogPostPageDescription $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts)
        nbTexts $
          H.preEscapedText $
            "\n<!--# include virtual=\"" <> virtual <> "\" -->"

prototypeRefliFooPage :: Bool -> Text -> MainHeaderTexts -> NavigationBlockTexts -> FooFormTexts -> Html
prototypeRefliFooPage autoreload url mhTexts@MainHeaderTexts {..} nbTexts fTexts = do
  refliDocument
    autoreload mainHeaderLanguage "" "" $ -- TODO page title, description, ...
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts)
        nbTexts $
          refliFooPageContent fTexts

prototypeRefliRunPage :: Bool -> Text -> MainHeaderTexts -> NavigationBlockTexts -> Html
prototypeRefliRunPage autoreload url mhTexts@MainHeaderTexts {..} nbTexts = do
  refliDocument
    autoreload mainHeaderLanguage "Run" "Run commands using the Refli CLI." $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts)
        nbTexts $
          refliRunPageContent

prototypeRefliCapturePage :: Bool -> Text -> MainHeaderTexts -> NavigationBlockTexts -> LandingPageCaptureFormTexts -> Html
prototypeRefliCapturePage autoreload url mhTexts@MainHeaderTexts {..} nbTexts cfTexts = do
  refliDocument
    autoreload mainHeaderLanguage "" "" $ -- TODO page title, description, ...
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts)
        nbTexts $
          refliCapturePageContent cfTexts

prototypeRefliLandingPage :: Bool -> Text -> MainHeaderTexts -> LandingPageTexts -> NavigationBlockTexts -> LandingPageCaptureFormTexts -> Html
prototypeRefliLandingPage autoreload url mhTexts@MainHeaderTexts {..} texts@LandingPageTexts {..} nbTexts cfTexts = do
  refliDocument
    autoreload landingPageLanguage landingPageTitle landingPageDescription $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts)
        nbTexts $
          refliLandingPageContent texts cfTexts

prototypeRefliLoginPage :: Bool -> Text -> MainHeaderTexts -> NavigationBlockTexts -> LoginFormTexts -> Html
prototypeRefliLoginPage autoreload url mhTexts@MainHeaderTexts {..} nbTexts lfTexts = do
  refliDocument
    autoreload "xx" "" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts)
        nbTexts $
          loginForm lfTexts

refliFooPageContent :: FooFormTexts -> Html
refliFooPageContent fTexts =
  div "max-48rem u-flow-c-4 u-space-after-c-4 center" $
    fooForm fTexts

refliRunPageContent :: Html
refliRunPageContent =
  div "max-48rem u-flow-c-4 u-space-after-c-4 center" $
    runForm

refliCapturePageContent :: LandingPageCaptureFormTexts -> Html
refliCapturePageContent cfTexts =
  div "max-48rem u-flow-c-4 u-space-after-c-4 center" $
    emailCaptureForm cfTexts

refliLandingPageContent :: LandingPageTexts -> LandingPageCaptureFormTexts -> Html
refliLandingPageContent LandingPageTexts {..} cfTexts = do
  H.style
    ".u-step-d-3 {\
    \  letter-spacing: 0;\
    \}"
  div "flow-all" $
    H.h1 ! A.class_ "u-step-d-3" $ H.text landingPageTitle
  div "switcher" $ do
    div "flow-all" $ do
      H.p $ H.text landingPageParagraph1
      H.p $ H.text landingPageParagraph2
      emailCaptureForm cfTexts
    H.div mempty

fooForm :: FooFormTexts -> Html
fooForm FooFormTexts {..} =
  div "box u-flow-c-4" $
    H.form ! A.class_ "c-text flow"
           ! A.method "POST"
           ! A.action "/echo/foo" $ do
      H.h4 $ H.text fooFormTitle
      H.div $ do
        H.label $ H.text fooFormFieldLabel
        H.input ! A.class_ "c-input"
                ! A.name "text-input"
                ! A.id "text-input"
                ! A.type_ "text"
                ! A.placeholder ""
        H.input ! A.type_ "hidden"
                ! A.name "current-language"
                ! A.id "current-language"
                ! A.value (H.toValue fooFormLanguage)
      H.button ! A.class_ "c-button c-button--primary" ! A.type_ "submit" $ do
        H.span $ H.text fooFormSubmit
        arrowRight

runForm :: Html
runForm =
  div "box u-flow-c-4" $
    H.form ! A.class_ "c-text flow"
           ! A.method "POST"
           ! A.action "/internal/a/run" $ do
      H.h4 $ H.text "Command-line interface"
      H.div $ do
        H.label $ H.text "Command"
        H.input ! A.class_ "c-input"
                ! A.name "command"
                ! A.id "command"
                ! A.type_ "text"
                ! A.placeholder ""
      H.button ! A.class_ "c-button c-button--primary" ! A.type_ "submit" $ do
        H.span $ H.text "Run"
        arrowRight

emailCaptureForm :: LandingPageCaptureFormTexts -> Html
emailCaptureForm LandingPageCaptureFormTexts {..} =
  div "box u-flow-c-4" $
    H.form ! A.class_ "c-text flow"
           ! A.method "POST"
           ! A.action "/a/subscribe" $ do
      H.h4 $ H.text landingPageCaptureFormTitle
      H.div $ do
        H.label $ H.text landingPageCaptureFormFieldLabel
        H.input ! A.class_ "c-input"
                ! A.name "email-address"
                ! A.id "email-address"
                ! A.type_ "text"
                ! A.placeholder ""
        H.input ! A.type_ "hidden"
                ! A.name "current-language"
                ! A.id "current-language"
                ! A.value (H.toValue landingPageCaptureFormLanguage)
      H.button ! A.class_ "c-button c-button--primary" ! A.type_ "submit" $ do
        H.span $ H.text landingPageCaptureFormSubmit
        arrowRight
      H.p $ H.text landingPageCaptureFormPrivacyNotice

prototypeRefliMessageSubscribeSuccess :: Bool -> Text -> MainHeaderTexts -> MessageSubscribeSuccessTexts -> NavigationBlockTexts -> Html
prototypeRefliMessageSubscribeSuccess autoreload url mhTexts@MainHeaderTexts {..} texts nbTexts = do
  refliDocument
    autoreload mainHeaderLanguage "Refli" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts)
        nbTexts $
          messageSubscribeSuccess texts

prototypeRefliMessageFooSuccess :: Bool -> Text -> MainHeaderTexts -> MessageFooSuccessTexts -> NavigationBlockTexts -> Html
prototypeRefliMessageFooSuccess autoreload url mhTexts@MainHeaderTexts {..} texts nbTexts = do
  refliDocument
    autoreload mainHeaderLanguage "Refli" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts)
        nbTexts $
          messageFooSuccess texts

messageSubscribeSuccess :: MessageSubscribeSuccessTexts -> Html
messageSubscribeSuccess MessageSubscribeSuccessTexts {..} =
  div "max-50rem u-flow-c-4 u-space-after-c-4 center" $
    div "u-container u-container-vertical" $
      div "c-text flow" $ do
        H.h2 $ H.text messageSubscribeSuccessTitle
        div "box c-text flow" $ do
          H.p $ H.text messageSubscribeSuccessParagraph1
          H.p $ H.preEscapedToMarkup messageSubscribeSuccessParagraph2
          H.p $ H.text messageSubscribeSuccessParagraph3

messageFooSuccess :: MessageFooSuccessTexts -> Html
messageFooSuccess MessageFooSuccessTexts {..} =
  div "max-50rem u-flow-c-4 u-space-after-c-4 center" $
    div "u-container u-container-vertical" $
      div "c-text flow" $ do
        H.h2 $ H.text messageFooSuccessTitle
        div "box c-text flow" $ do
          H.p $ H.text messageFooSuccessParagraph1

prototypeRefliMessageRunResult :: Bool -> Text -> MainHeaderTexts -> NavigationBlockTexts -> Text -> Text -> Html
prototypeRefliMessageRunResult autoreload url mhTexts@MainHeaderTexts {..} nbTexts cmd content = do
  refliDocument
    autoreload mainHeaderLanguage "Run" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts)
        nbTexts $ do
          div "u-space-after-c-4" $
            div "u-container-vertical" $
              div "c-text flow" $ do
                H.h4 $ "Command-line interface"
                H.pre $
                  H.code $ do
                    let content' = "$ refli " <> cmd <> "\n" <> content
                    H.text content'

loginForm :: LoginFormTexts -> Html
loginForm LoginFormTexts {..} = do
  div "max-48rem u-flow-c-4 u-space-after-c-4 center " $
    H.form $ do
      div "u-container u-container-vertical bordered-3" $
        div "c-text flow" $ do
          H.h2 $ H.text loginFormTitle
          H.div $ do
            H.label $ H.text loginFormFieldLabel1
            H.input ! A.class_ "c-input"
          H.div $ do
            H.label $ H.text loginFormFieldLabel2
            H.input ! A.class_ "c-input"
          H.div $
            H.a ! A.href "reset.html" $ H.text loginFormLink1
      div "switcher-0px" $ do
        H.button ! A.class_ "c-button c-button--primary c-button--tall"
                 ! A.type_ "submit" $ do
          H.span $ H.text loginFormSubmit
          arrowRight
        H.a ! A.class_ "c-button c-button--secondary c-button--tall"
            ! A.href "signup.html" $
          H.span $ H.text loginFormLink2

--------------------------------------------------------------------------------
data MainHeaderTexts = MainHeaderTexts
  { mainHeaderLanguage :: Text
  , mainHeaderLinkBlog :: Text
  , mainHeaderLinkPlayground :: Text
  , mainHeaderLinkComputeSalaries :: Text
  , mainHeaderLinkDocumentation :: Text
  }

data NavigationBlockTexts = NavigationBlockTexts
  { navigationBlockLanguage :: Text
  , navigationBlockDocumentation :: Text
  }

prototypeRefliPage :: Text -> Text -> Html -> NavigationBlockTexts -> Html -> Html
prototypeRefliPage lang url header NavigationBlockTexts {..} content =
  H.body ! A.class_ "u-container-vertical" $ do
    H.header $
      div "u-container" $
        div "u-bar" $ do
          div "u-bar__left" $
            H.a ! A.href (H.toValue $ "/" <> lang) $
              H.img ! A.src "/static/images/logo.svg" ! A.alt "Refli"
          div "u-bar__right" $
            header

    H.main $
      div "u-container" $
        content

    H.footer $
      div "u-container" $ do
        H.hr
        div "switcher" $
          div "c-content flow" $ do
            H.h4 "Refli"
            H.ul ! A.class_ "no-disc" $ do
              H.li $
                H.a ! A.href (H.toValue $ "/" <> lang <> "/about") $ "About"
              H.li $
                H.a ! A.href (H.toValue $ "/" <> lang <> "/blog") $ "Blog"
              H.li $
                H.a ! A.href (H.toValue $ "/" <> lang <> "/documentation") $
                  H.text navigationBlockDocumentation
              H.li $
                H.a ! A.href (H.toValue $ "/" <> lang <> "/contact") $ "Contact"
              H.li $
                H.a ! A.href (H.toValue $ "/" <> lang <> "/disclaimer") $ "Disclaimer"
              H.li $
                H.a ! A.href "/changelog" $ "Changelog"
              H.li $
                H.a ! A.href "/fr/lex" $ "Lex Iterata" -- TODO Translate.
            H.hr
            H.ul ! A.class_ "no-disc horizontal" $ do
              H.li $
                H.a ! A.href (H.toValue $ "/en" <> url) $ "EN"
              H.li $
                H.a ! A.href (H.toValue $ "/fr" <> url) $ "FR"
              H.li $
                H.a ! A.href (H.toValue $ "/nl" <> url) $ "NL"

        div "flow u-flow-c-4" $
          H.span "© Hypered SRL, 2023-2024."

prototypeRefliMainHeader :: MainHeaderTexts -> Html
prototypeRefliMainHeader MainHeaderTexts {..} = do
  let linkBlog = "/" <> mainHeaderLanguage <> "/blog"
  H.ul $ do
    H.li $
      div "menu-item" $
        H.a ! A.href (H.toValue linkBlog) ! A.class_ "menu-link" $
          H.text mainHeaderLinkBlog

    H.li $
      div "menu-item" ! A.tabindex "-1" $ do
        H.i ! A.class_ "menu-mask" ! A.tabindex "-1" $ mempty
        H.a ! A.class_ "menu-dropdown" $ H.text mainHeaderLinkPlayground
        div "menu-dropdown-content" $
          H.a ! A.href (H.toValue $ "/" <> mainHeaderLanguage <> "/describe") $
            H.text mainHeaderLinkComputeSalaries

    H.li $
      div "menu-item" $
        H.a ! A.href (H.toValue $ "/" <> mainHeaderLanguage <> "/documentation")
            ! A.class_ "menu-link" $
          H.text mainHeaderLinkDocumentation
