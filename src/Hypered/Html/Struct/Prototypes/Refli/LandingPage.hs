module Hypered.Html.Struct.Prototypes.Refli.LandingPage where

import qualified Data.Text.Lazy as TL
import Hypered.Html.Helpers
import Hypered.Html.Struct.Icons (arrowRight)
import Hypered.Html.Struct.Prototypes.Refli.Common as Struct
import Protolude hiding (div)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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

data SignupFormTexts = SignupFormTexts
  { signupFormLanguage :: Text
  , signupFormTitle :: Text
  , signupFormFieldLabel1 :: Text
  , signupFormFieldLabel2 :: Text
  , signupFormLink :: Text
  , signupFormSubmit :: Text
  }

data MessageSignupSuccessTexts = MessageSignupSuccessTexts
  { messageSignupSuccessLanguage :: Text
  , messageSignupSuccessTitle :: Text
  , messageSignupSuccessParagraph1 :: Text
  }

data ResetPasswordFormTexts = ResetPasswordFormTexts
  { resetPasswordFormLanguage :: Text
  , resetPasswordFormTitle :: Text
  , resetPasswordFormFieldLabel :: Text
  , resetPasswordFormLink :: Text
  , resetPasswordFormSubmit :: Text
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
prototypeRefliRootPage :: RefliDocumentOptions -> MainHeaderTexts -> LandingPageTexts -> NavigationBlockTexts -> Html
prototypeRefliRootPage opts mhTexts@MainHeaderTexts {..} LandingPageTexts {..} nbTexts = do
  refliDocument
    opts mainHeaderLanguage "Refli" landingPageDescription $
      prototypeRefliPage
        mainHeaderLanguage
        ""
        (prototypeRefliMainNav' mhTexts)
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
prototypeRefliDescribeFormPage :: RefliDocumentOptions -> Text -> MainHeaderTexts -> DescribeFormPageTexts  -> NavigationBlockTexts -> Html
prototypeRefliDescribeFormPage opts url mhTexts@MainHeaderTexts {..} DescribeFormPageTexts {..} nbTexts = do
  refliDocument
    opts mainHeaderLanguage describeFormPageTitle describeFormPageDescription $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
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

prototypeRefliEchoPage :: RefliDocumentOptions -> Text -> MainHeaderTexts -> NavigationBlockTexts -> TL.Text -> Html
prototypeRefliEchoPage opts url mhTexts@MainHeaderTexts {..} nbTexts content = do
  refliDocument
    opts mainHeaderLanguage "Echo" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $ do
          standardSmallBox $ do
            H.h4 $ "Echo"
            H.pre $
              H.code $
                H.lazyText content

-- Similar to the echo page, but for Forming forms.
prototypeRefliSubmitPage :: RefliDocumentOptions -> Text -> MainHeaderTexts -> NavigationBlockTexts -> Html -> Html
prototypeRefliSubmitPage opts url mhTexts@MainHeaderTexts {..} nbTexts content = do
  refliDocument
    opts mainHeaderLanguage "Echo" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $
          standardSmallBox $ do
            H.h4 $ "Submittal"
            content

-- Similar to the echo page, but for Playground forms.
prototypeRefliSubmitPlaygroundPage :: RefliDocumentOptions -> Text -> Text -> Text -> Text -> Text -> MainHeaderTexts -> NavigationBlockTexts -> Html -> Html
prototypeRefliSubmitPlaygroundPage opts name symbol formTitle conceptUrl url mhTexts@MainHeaderTexts {..} nbTexts content = do
  refliDocument
    opts mainHeaderLanguage ("Playground - " <> name) "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $ do
          H.div ! A.class_ "c-content flow-all" $ do
            H.h1 $ H.text name
            H.small ! A.class_ "breadcrumb" $ do
              H.text "Computation "
              H.code $ H.text symbol
          standardSmallBox $ do
            H.h4 $ H.text formTitle
            content
          H.div ! A.class_ "c-content flow-all paragraph-70ch" $
            H.p $ H.a ! A.href (H.toValue conceptUrl) $ H.text "View concepts"

standardSmallBox :: Html -> Html
standardSmallBox content =
  div "max-30rem u-flow-c-4 u-space-after-c-4 center" $
    div "box u-flow-c-4" $
      div "c-text flow" $
        content

-- Use this "standard small" form function in other functions in this file.
standardSmallForm :: Text -> Html -> Html
standardSmallForm action content =
  div "max-30rem u-flow-c-4 u-space-after-c-4 center" $
    div "box u-flow-c-4" $
      H.form ! A.class_ "c-text flow"
             ! A.method "POST"
             ! A.action (H.toValue action) $
        content

standardSmallPlaygroundForm :: Text -> Text -> Text -> Text -> Html -> Html
standardSmallPlaygroundForm name symbol url action content = do
  H.div ! A.class_ "c-content flow-all" $ do
    H.h1 $ H.text name
    H.small ! A.class_ "breadcrumb" $ do
      H.text $ "Computation "
      H.code $ H.text symbol
  standardSmallForm action content
  H.div ! A.class_ "c-content flow-all paragraph-70ch" $
    H.p $ H.a ! A.href (H.toValue url) $ H.text "View concepts"

-- Use this "base" page in the other functions in this file.
prototypeRefliBasePage :: RefliDocumentOptions -> Text -> Maybe Profile -> MainHeaderTexts -> NavigationBlockTexts -> Html -> Html
prototypeRefliBasePage opts url mprofile mhTexts@MainHeaderTexts {..} nbTexts content = do
  refliDocument
    opts mainHeaderLanguage "" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav mprofile mhTexts)
        nbTexts $
          content

-- Move elsewhere.
prototypeRefliBlogIndexPage :: RefliDocumentOptions -> Text -> MainHeaderTexts -> BlogPostPageTexts -> NavigationBlockTexts -> Html
prototypeRefliBlogIndexPage opts url mhTexts@MainHeaderTexts {..} BlogPostPageTexts {..} nbTexts = do
  refliDocument
    opts mainHeaderLanguage "Blog" "" $ -- TODO Description.
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
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

-- Move elsewhere.
data BlogPostPageTexts = BlogPostPageTexts
  { blogPostPageLanguage :: Text
  , blogPostPageTitle :: Text
  , blogPostPageDescription :: Text
  , blogPostReadMore :: Text -- Use on the blog index, not the post.
  }

-- Move elsewhere.
prototypeRefliBlogPostPage :: RefliDocumentOptions -> Text -> MainHeaderTexts -> BlogPostPageTexts -> Text -> NavigationBlockTexts -> Html
prototypeRefliBlogPostPage opts url mhTexts@MainHeaderTexts {..} BlogPostPageTexts {..} virtual nbTexts = do
  refliDocument
    opts blogPostPageLanguage blogPostPageTitle blogPostPageDescription $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $
          H.preEscapedText $
            "\n<!--# include virtual=\"" <> virtual <> "\" -->"

prototypeRefliFooPage :: RefliDocumentOptions -> Text -> MainHeaderTexts -> NavigationBlockTexts -> FooFormTexts -> Html
prototypeRefliFooPage opts url mhTexts@MainHeaderTexts {..} nbTexts fTexts = do
  refliDocument
    opts mainHeaderLanguage "" "" $ -- TODO page title, description, ...
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $
          refliFooPageContent fTexts

prototypeRefliRunPage :: RefliDocumentOptions -> Text -> MainHeaderTexts -> NavigationBlockTexts -> Html
prototypeRefliRunPage opts url mhTexts@MainHeaderTexts {..} nbTexts = do
  refliDocument
    opts mainHeaderLanguage "Run" "Run commands using the Refli CLI." $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $
          refliRunPageContent

prototypeRefliCapturePage :: RefliDocumentOptions -> Text -> MainHeaderTexts -> NavigationBlockTexts -> LandingPageCaptureFormTexts -> Text -> Html
prototypeRefliCapturePage opts url mhTexts@MainHeaderTexts {..} nbTexts cfTexts action = do
  refliDocument
    opts mainHeaderLanguage "" "" $ -- TODO page title, description, ...
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $
          refliCapturePageContent cfTexts action

prototypeRefliLandingPage :: RefliDocumentOptions -> Text -> MainHeaderTexts -> LandingPageTexts -> NavigationBlockTexts -> LandingPageCaptureFormTexts -> Html
prototypeRefliLandingPage opts url mhTexts@MainHeaderTexts {..} texts@LandingPageTexts {..} nbTexts cfTexts = do
  refliDocument
    opts landingPageLanguage landingPageTitle landingPageDescription $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $
          refliLandingPageContent texts cfTexts

prototypeRefliSignupPage :: RefliDocumentOptions -> Text -> MainHeaderTexts -> NavigationBlockTexts -> SignupFormTexts -> Text -> Html
prototypeRefliSignupPage opts url mhTexts@MainHeaderTexts {..} nbTexts sfTexts action = do
  refliDocument
    opts "xx" "" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $
          signupForm sfTexts action

prototypeRefliResetPasswordPage :: RefliDocumentOptions -> Text -> MainHeaderTexts -> NavigationBlockTexts -> ResetPasswordFormTexts -> Text -> Html
prototypeRefliResetPasswordPage opts url mhTexts@MainHeaderTexts {..} nbTexts rpfTexts action = do
  refliDocument
    opts "xx" "" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $
          resetPasswordForm rpfTexts action

prototypeRefliLoginPage :: RefliDocumentOptions -> Text -> MainHeaderTexts -> NavigationBlockTexts -> LoginFormTexts -> Text -> Html
prototypeRefliLoginPage opts url mhTexts@MainHeaderTexts {..} nbTexts lfTexts action = do
  refliDocument
    opts "xx" "" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $
          loginForm lfTexts action

-- HTTP status code page.
prototypeRefliError400Page :: RefliDocumentOptions -> Text -> Html
prototypeRefliError400Page opts err = do
  prototypeRefliErrorPageLang opts title400 $ do
    H.p $ H.code $ H.text err
    H.p $ H.text $ "EN: " <> message400En
    H.p $ H.text $ "FR: " <> message400Fr
    H.p $ H.text $ "NL: " <> message400Nl

prototypeRefliError401PageEn :: RefliDocumentOptions -> Html
prototypeRefliError401PageEn opts = do
  prototypeRefliErrorPageLang opts title401 $
    H.p $ message401En "en"

prototypeRefliError401PageFr :: RefliDocumentOptions -> Html
prototypeRefliError401PageFr opts = do
  prototypeRefliErrorPageLang opts title401 $
    H.p $ message401Fr "fr"

prototypeRefliError401PageNl :: RefliDocumentOptions -> Html
prototypeRefliError401PageNl opts = do
  prototypeRefliErrorPageLang opts title401 $
    H.p $ message401Nl "nl"

prototypeRefliError401Page :: RefliDocumentOptions -> Html
prototypeRefliError401Page opts = do
  prototypeRefliErrorPageLang opts title401 $ do
    H.p $ do
      H.text $ "EN: "
      message401En "en" -- Is it really useful to have this lang argument ?
    H.p $ do
      H.text $ "FR: "
      message401Fr "fr"
    H.p $ do
      H.text $ "NL: "
      message401Nl "nl"

prototypeRefliError404PageEn :: RefliDocumentOptions -> Html
prototypeRefliError404PageEn opts = do
  prototypeRefliErrorPageLang opts title404 $
    H.p $ H.text message404En

prototypeRefliError404PageFr :: RefliDocumentOptions -> Html
prototypeRefliError404PageFr opts = do
  prototypeRefliErrorPageLang opts title404 $
    H.p $ H.text message404Fr

prototypeRefliError404PageNl :: RefliDocumentOptions -> Html
prototypeRefliError404PageNl opts = do
  prototypeRefliErrorPageLang opts title404 $
    H.p $ H.text message404Nl

prototypeRefliError404Page :: RefliDocumentOptions -> Html
prototypeRefliError404Page opts = do
  prototypeRefliErrorPageLang opts title404 $ do
    H.p $ H.text $ "EN: " <> message404En
    H.p $ H.text $ "FR: " <> message404Fr
    H.p $ H.text $ "NL: " <> message404Nl

prototypeRefliError500PageEn :: RefliDocumentOptions -> Html
prototypeRefliError500PageEn opts = do
  prototypeRefliErrorPageLang opts title500 $
    H.p $ H.text message500En

prototypeRefliError500PageFr :: RefliDocumentOptions -> Html
prototypeRefliError500PageFr opts = do
  prototypeRefliErrorPageLang opts title500 $
    H.p $ H.text message500Fr

prototypeRefliError500PageNl :: RefliDocumentOptions -> Html
prototypeRefliError500PageNl opts = do
  prototypeRefliErrorPageLang opts title500 $
    H.p $ H.text message500Nl

prototypeRefliError500Page :: RefliDocumentOptions -> Html
prototypeRefliError500Page opts = do
  prototypeRefliErrorPageLang opts title500 $ do
    H.p $ H.text $ "EN: " <> message500En
    H.p $ H.text $ "FR: " <> message500Fr
    H.p $ H.text $ "NL: " <> message500Nl

prototypeRefliErrorPageLang :: RefliDocumentOptions -> Text -> Html -> Html
prototypeRefliErrorPageLang opts title content = do
  refliDocument
    opts "en" "" "" $ -- TODO page title, description, ...
      prototypeRefliCenterPage $
        div "max-50rem u-flow-c-4 u-space-after-c-4 center" $
          div "u-container u-container-vertical" $
            div "c-text .flow" $
              div "box c-text flow" $ do
                H.a ! A.href "/" $
                    H.img ! A.src "/static/images/logo.svg" ! A.alt "Refli"
                H.h2 $ H.text title
                content

title400, message400En, message400Fr, message400Nl :: Text
title400 = "400 Bad Request"

message400En = "Our server cannot process the request. This may be due to several reasons, including a bad request syntax, a missing field, a value of the wrong type, a size that is too large, or an invalid request message framing."

message400Fr = "Notre serveur ne peut pas traiter la demande. Cela peut être dû à plusieurs raisons, notamment une mauvaise syntaxe de la demande, un champ manquant, une valeur d'un type incorrect, une taille trop importante ou un cadre de message de demande non valide."

message400Nl = "Onze server kan het verzoek niet verwerken. Dit kan verschillende redenen hebben, waaronder een slechte verzoeksyntaxis, een ontbrekend veld, een waarde van het verkeerde type, een te grote grootte of een ongeldige framing van het verzoekbericht."

title401 :: Text
message401En, message401Fr, message401Nl :: Text -> Html
title401 = "401 Unauthorized"

message401En lang = do
  H.text "The email address or password you entered is incorrect. "
  H.a ! A.href (H.toValue $ "/" <> lang <> "/login") $ H.text "Please try again"
  H.text ". If necessary, you can "
  H.a ! A.href (H.toValue $ "/" <> lang <> "/reset") $ H.text "reset your password"
  H.text ". You can also "
  H.a ! A.href (H.toValue $ "/" <> lang <> "/contact") $ H.text "contact us"
  H.text "."

message401Fr lang = do
  H.text "L'adresse email ou le mot de passe que vous avez saisi est incorrect. "
  H.a ! A.href (H.toValue $ "/" <> lang <> "/login") $ H.text "Veuillez réessayer"
  H.text ". Si nécessaire, vous pouvez "
  H.a ! A.href (H.toValue $ "/" <> lang <> "/reset") $ H.text "réinitialiser votre mot de passe"
  H.text ". Vous pouvez également "
  H.a ! A.href (H.toValue $ "/" <> lang <> "/contact") $ H.text "nous contacter"
  H.text "."

message401Nl lang = do
  H.text "Het ingevoerde e-mailadres of wachtwoord is onjuist. "
  H.a ! A.href (H.toValue $ "/" <> lang <> "/contact") $ H.text "Probeer het opnieuw"
  H.text ". Indien nodig kunt u uw "
  H.a ! A.href (H.toValue $ "/" <> lang <> "/reset") $ H.text "wachtwoord opnieuw instellen"
  ". U kunt ook "
  H.a ! A.href (H.toValue $ "/" <> lang <> "/contact") $ H.text "contact met ons opnemen"
  "."

title404, message404En, message404Fr, message404Nl :: Text
title404 = "404 Not Found"

message404En = "The resource you requested was not found at this URL."

message404Fr = "La ressource demandée n'a pas été trouvée à cette URL."

message404Nl = "De door u opgevraagde bron is niet gevonden op deze URL."

title500, message500En, message500Fr, message500Nl :: Text
title500 = "500 Internal Server Error"

message500En = "Our server experienced an unexpected condition. Please try to refresh the page or come back later. We apologize for any inconvenience."

message500Fr = "Notre serveur a rencontré une situation inattendue. Veuillez rafraîchir la page ou revenir plus tard. Nous vous prions de nous excuser pour la gêne occasionnée."

message500Nl = "Onze server ondervond een onverwachte omstandigheid. Probeer de pagina te vernieuwen of kom later terug. Onze excuses voor het ongemak."

refliFooPageContent :: FooFormTexts -> Html
refliFooPageContent fTexts =
  div "max-30rem u-flow-c-4 u-space-after-c-4 center" $
    fooForm fTexts

refliRunPageContent :: Html
refliRunPageContent =
  div "max-30rem u-flow-c-4 u-space-after-c-4 center" $
    runForm

refliCapturePageContent :: LandingPageCaptureFormTexts -> Text -> Html
refliCapturePageContent cfTexts action =
  div "max-30rem u-flow-c-4 u-space-after-c-4 center" $
    emailCaptureForm cfTexts action

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
      emailCaptureForm cfTexts "/a/subscribe"
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

-- Form that post a "b" containg a string, to trigger an error in either a
-- handler that expects an "a", or a handler that expects a "b" but as an
-- integer.
-- TODO Make it inline (i.e. appear just as a link).
aForm :: Text -> Html
aForm action =
  H.form ! A.class_ "c-text flow"
         ! A.method "POST"
         ! A.action (H.toValue action) $ do
    H.div $ do
      H.input ! A.type_ "hidden"
              ! A.name "b"
              ! A.id "b"
              ! A.value "b"
    H.button ! A.class_ "c-button c-button--primary" ! A.type_ "submit" $ do
      H.span $ H.text "Submit"
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

emailCaptureForm :: LandingPageCaptureFormTexts -> Text -> Html
emailCaptureForm LandingPageCaptureFormTexts {..} action =
  div "box u-flow-c-4" $
    H.form ! A.class_ "c-text flow"
           ! A.method "POST"
           ! A.action (H.toValue action) $ do
      H.h4 $ H.text landingPageCaptureFormTitle
      H.div $ do
        H.label ! A.for "email-address" $ H.text landingPageCaptureFormFieldLabel
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

prototypeRefliMessageSubscribeSuccess :: RefliDocumentOptions -> Text -> MainHeaderTexts -> MessageSubscribeSuccessTexts -> NavigationBlockTexts -> Html
prototypeRefliMessageSubscribeSuccess opts url mhTexts@MainHeaderTexts {..} texts nbTexts = do
  refliDocument
    opts mainHeaderLanguage "Refli" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $
          messageSubscribeSuccess texts

prototypeRefliMessageFooSuccess :: RefliDocumentOptions -> Text -> MainHeaderTexts -> MessageFooSuccessTexts -> NavigationBlockTexts -> Html
prototypeRefliMessageFooSuccess opts url mhTexts@MainHeaderTexts {..} texts nbTexts = do
  refliDocument
    opts mainHeaderLanguage "Refli" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $
          messageFooSuccess texts

prototypeRefliMessageSignupSuccess :: RefliDocumentOptions -> Text -> MainHeaderTexts -> MessageSignupSuccessTexts -> NavigationBlockTexts -> Html
prototypeRefliMessageSignupSuccess opts url mhTexts@MainHeaderTexts {..} texts nbTexts = do
  refliDocument
    opts mainHeaderLanguage "Refli" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $
          messageSignupSuccess texts

messageSubscribeSuccess :: MessageSubscribeSuccessTexts -> Html
messageSubscribeSuccess MessageSubscribeSuccessTexts {..} =
  div "max-50rem u-flow-c-4 u-space-after-c-4 center" $
    div "u-container u-container-vertical" $
      div "c-text flow" $
        div "box c-text flow" $ do
          H.h2 $ H.text messageSubscribeSuccessTitle
          H.p $ H.text messageSubscribeSuccessParagraph1
          H.p $ H.preEscapedToMarkup messageSubscribeSuccessParagraph2
          H.p $ H.text messageSubscribeSuccessParagraph3

messageFooSuccess :: MessageFooSuccessTexts -> Html
messageFooSuccess MessageFooSuccessTexts {..} =
  div "max-50rem u-flow-c-4 u-space-after-c-4 center" $
    div "u-container u-container-vertical" $
      div "c-text flow" $
        div "box c-text flow" $ do
          H.h2 $ H.text messageFooSuccessTitle
          H.p $ H.text messageFooSuccessParagraph1

messageSignupSuccess :: MessageSignupSuccessTexts -> Html
messageSignupSuccess MessageSignupSuccessTexts {..} =
  div "max-50rem u-flow-c-4 u-space-after-c-4 center" $
    div "u-container u-container-vertical" $
      div "c-text flow" $
        div "box c-text flow" $ do
          H.h2 $ H.text messageSignupSuccessTitle
          H.p $ H.preEscapedToMarkup messageSignupSuccessParagraph1

prototypeRefliMessageRunResult :: RefliDocumentOptions -> Text -> MainHeaderTexts -> NavigationBlockTexts -> Text -> Text -> Html
prototypeRefliMessageRunResult opts url mhTexts@MainHeaderTexts {..} nbTexts cmd content = do
  refliDocument
    opts mainHeaderLanguage "Run" "" $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainNav' mhTexts)
        nbTexts $ do
          div "u-space-after-c-4" $
            div "u-container-vertical" $
              div "c-text flow" $ do
                H.h4 $ "Command-line interface"
                H.pre $
                  H.code $ do
                    let content' = "$ refli " <> cmd <> "\n" <> content
                    H.text content'

signupForm :: SignupFormTexts -> Text -> Html
signupForm SignupFormTexts {..} action = do
  div "max-30rem u-flow-c-4 u-space-after-c-4 center " $
    H.form ! A.method "POST"
           ! A.action (H.toValue action) $ do
      div "u-container u-container-vertical bordered-3" $
        div "c-text flow" $ do
          H.h2 $ H.text signupFormTitle
          H.div $ do
            H.label ! A.for "email-address" $ H.text signupFormFieldLabel1
            H.input ! A.type_ "text"
                    ! A.class_ "c-input"
                    ! A.placeholder ""
                    ! A.name "email-address"
                    ! A.id "email-address"
          H.div $ do
            H.label ! A.for "password" $ H.text signupFormFieldLabel2
            H.input ! A.type_ "password"
                    ! A.class_ "c-input"
                    ! A.placeholder ""
                    ! A.name "password"
                    ! A.id "password"
          H.input ! A.type_ "hidden"
                  ! A.name "current-language"
                  ! A.id "current-language"
                  ! A.value (H.toValue signupFormLanguage)
      div "switcher-0px" $ do
        H.button ! A.class_ "c-button c-button--primary c-button--tall"
                 ! A.type_ "submit" $ do
          H.span $ H.text signupFormSubmit
          arrowRight
        H.a ! A.class_ "c-button c-button--secondary c-button--tall"
            ! A.href "login" $
          H.span $ H.text signupFormLink

resetPasswordForm :: ResetPasswordFormTexts -> Text -> Html
resetPasswordForm ResetPasswordFormTexts {..} action = do
  div "max-30rem u-flow-c-4 u-space-after-c-4 center " $
    H.form ! A.method "POST"
           ! A.action (H.toValue action) $ do
      div "u-container u-container-vertical bordered-3" $
        div "c-text flow" $ do
          H.h2 $ H.text resetPasswordFormTitle
          H.div $ do
            H.label ! A.for "email-address" $ H.text resetPasswordFormFieldLabel
            H.input ! A.type_ "text"
                    ! A.class_ "c-input"
                    ! A.placeholder ""
                    ! A.name "email-address"
                    ! A.id "email-address"
          H.input ! A.type_ "hidden"
                  ! A.name "current-language"
                  ! A.id "current-language"
                  ! A.value (H.toValue resetPasswordFormLanguage)
      div "switcher-0px" $ do
        H.button ! A.class_ "c-button c-button--primary c-button--tall"
                 ! A.type_ "submit" $ do
          H.span $ H.text resetPasswordFormSubmit
          arrowRight
        H.a ! A.class_ "c-button c-button--secondary c-button--tall"
            ! A.href "login" $
          H.span $ H.text resetPasswordFormLink

loginForm :: LoginFormTexts -> Text -> Html
loginForm LoginFormTexts {..} action = do
  div "max-30rem u-flow-c-4 u-space-after-c-4 center " $
    H.form ! A.method "POST"
           ! A.action (H.toValue action) $ do
      div "u-container u-container-vertical bordered-3" $
        div "c-text flow" $ do
          H.h2 $ H.text loginFormTitle
          H.div $ do
            H.label ! A.for "email-address" $ H.text loginFormFieldLabel1
            H.input ! A.type_ "text"
                    ! A.class_ "c-input"
                    ! A.placeholder ""
                    ! A.name "email-address"
                    ! A.id "email-address"
          H.div $ do
            H.label ! A.for "password" $ H.text loginFormFieldLabel2
            H.input ! A.type_ "password"
                    ! A.class_ "c-input"
                    ! A.placeholder ""
                    ! A.name "password"
                    ! A.id "password"
          H.input ! A.type_ "hidden"
                  ! A.name "current-language"
                  ! A.id "current-language"
                  ! A.value (H.toValue loginFormLanguage)
          H.div $
            H.a ! A.href "login" $ H.text loginFormLink1
      div "switcher-0px" $ do
        H.button ! A.class_ "c-button c-button--primary c-button--tall"
                 ! A.type_ "submit" $ do
          H.span $ H.text loginFormSubmit
          arrowRight
        H.a ! A.class_ "c-button c-button--secondary c-button--tall"
            ! A.href "signup" $
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
prototypeRefliPage lang url header nbTexts content =
  H.body ! A.class_ "u-container-vertical" $ do
    prototypeRefliHeader lang header

    H.main $
      div "u-container" $
        content

    prototypeRefliFooter lang url nbTexts

prototypeRefliHeader :: Text -> Html -> Html
prototypeRefliHeader lang header =
    H.header $
      div "u-container" $
        div "u-bar u-bar--top-aligned" $ do
          div "u-bar__left" $
            div "menu-item-height" $
              H.a ! A.href (H.toValue $ "/" <> lang) $
                H.img ! A.src "/static/images/logo.svg" ! A.alt "Refli"
          div "u-bar__right" $
            header

prototypeRefliFooter :: Text -> Text -> NavigationBlockTexts -> Html
prototypeRefliFooter lang url NavigationBlockTexts {..} =
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

        div "flow u-flow-c-4" $ do
          H.img ! A.src "/static/images/belgium-flag.svg"
                ! A.alt "The flag of Belgium"
                ! A.width "15px"
                ! A.height "13px"
          H.span $ do
            H.preEscapedText "&nbsp; &nbsp; &nbsp; © "
            H.a ! A.class_ "normal-link"
                ! A.href "https://hypered.be/en" $
              "Hypered SRL"
            H.text ", 2023-2025."

-- User profile informaton relevant to the UI.
data Profile = Profile
  { profileEmail :: Text -- ^ Email address (we currently use that instead of username)
  }
  deriving (Eq, Show)

prototypeRefliMainNav :: Maybe Profile -> MainHeaderTexts ->Html
prototypeRefliMainNav mprofile MainHeaderTexts {..} = do
  let linkBlog = "/" <> mainHeaderLanguage <> "/blog"
      linkLex = "/" <> mainHeaderLanguage <> "/lex"
  H.ul $ do
    H.li $
      div "menu-item" $
        H.a ! A.href (H.toValue linkBlog) ! A.class_ "menu-link" $
          H.text mainHeaderLinkBlog

    H.li $
      div "menu-item" $
        H.a ! A.href (H.toValue linkLex) ! A.class_ "menu-link" $
          H.text "Lex"

    H.li $
      div "menu-item" ! A.tabindex "-1" $ do
        H.i ! A.class_ "menu-mask" ! A.tabindex "-1" $ mempty
        H.a ! A.class_ "menu-dropdown" $ H.text mainHeaderLinkPlayground
        div "menu-dropdown-content" $
          H.a ! A.href (H.toValue $ "/" <> mainHeaderLanguage <> "/describe") $
            H.text mainHeaderLinkComputeSalaries

    maybe
      mempty
      ( \Profile {..} ->
          H.li $ do
            div "menu-item" ! A.tabindex "-1" $ do
              H.i ! A.class_ "menu-mask" ! A.tabindex "-1" $ mempty
              H.a ! A.class_ "menu-dropdown" $ H.text "Me" -- TODO Avatar image.
              div "menu-dropdown-content" $ do
                -- TODO Should not be a link.
                -- TODO Should be the last H.li.
                H.a ! A.href "#" $ do
                  H.text $ "Signed in as"
                  H.br
                  H.text profileEmail
                H.a ! A.href (H.toValue $ "/" <> mainHeaderLanguage <> "/logout") $
                  H.text "Sign out"
                H.a ! A.href (H.toValue $ "/" <> mainHeaderLanguage <> "/settings/profile") $
                  H.text "Settings"
      )
      mprofile

    H.li $
      div "menu-item" $
        H.a ! A.href (H.toValue $ "/" <> mainHeaderLanguage <> "/documentation")
            ! A.class_ "menu-link" $
          H.text mainHeaderLinkDocumentation

prototypeRefliMainNav' :: MainHeaderTexts -> Html
prototypeRefliMainNav' = prototypeRefliMainNav Nothing

prototypeRefliCenterPage :: Html -> Html
prototypeRefliCenterPage content =
  H.body ! A.class_ "u-container-vertical" $ do
    H.header mempty

    H.main ! A.class_ "center" $
      content

    H.footer mempty
