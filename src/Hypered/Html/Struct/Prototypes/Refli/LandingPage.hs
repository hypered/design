module Hypered.Html.Struct.Prototypes.Refli.LandingPage where

import qualified Data.Text as T
import Hypered.Html.Common (
  autoReload,
 )
import Hypered.Html.Helpers
import Hypered.Html.Struct.Prototypes.Refli.Common as Struct
import Protolude hiding (div)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.HTML.TagSoup (innerText, Tag(..), (~==))

--------------------------------------------------------------------------------
data LandingPageTexts = LandingPageTexts
  { landingPageLanguage :: Text
  , landingPageTitle :: Text
  , landingPageDescription :: Text
  , landingPageParagraph1 :: Text
  , landingPageParagraph2 :: Text
  }

-- Move elsewhere.
data BlogPostPageTexts = BlogPostPageTexts
  { blogPostPageLanguage :: Text
  , blogPostPageTitle :: Text
  , blogPostPageDescription :: Text
  }

-- Move elsewhere.
prototypeRefliBlogPostPage :: Bool -> Text -> MainHeaderTexts -> BlogPostPageTexts -> Text -> Html
prototypeRefliBlogPostPage autoreload url mhTexts@MainHeaderTexts {..} BlogPostPageTexts {..} virtual = do
  refliDocument
    autoreload blogPostPageLanguage blogPostPageTitle blogPostPageDescription $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts) $
          H.preEscapedText $
            "\n<!--# include virtual=\"" <> virtual <> "\" -->"

prototypeRefliLandingPage :: Bool -> Text -> MainHeaderTexts -> LandingPageTexts -> Html
prototypeRefliLandingPage autoreload url mhTexts@MainHeaderTexts {..} texts@LandingPageTexts {..} = do
  refliDocument
    autoreload landingPageLanguage landingPageTitle landingPageDescription $
      prototypeRefliPage
        mainHeaderLanguage
        url
        (prototypeRefliMainHeader mhTexts) $
          refliLandingPageContent texts

refliLandingPageContent :: LandingPageTexts -> Html
refliLandingPageContent LandingPageTexts {..} = do
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
    H.div mempty

--------------------------------------------------------------------------------
data MainHeaderTexts = MainHeaderTexts
  { mainHeaderLanguage :: Text
  , mainHeaderLinkBlog :: Text
  , mainHeaderLinkPlayground :: Text
  , mainHeaderLinkComputeSalaries :: Text
  , mainHeaderLinkDocumentation :: Text
  }

prototypeRefliPage :: Text -> Text -> Html -> Html -> Html
prototypeRefliPage lang url header content =
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
          div "c-content flow" $
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
          H.a ! A.href "/fr/describe" $ -- TODO Translate.
            H.text mainHeaderLinkComputeSalaries

    H.li $
      div "menu-item" $
        H.a ! A.href "/fr/documentation" ! A.class_ "menu-link" $ -- TODO Translate.
          H.text mainHeaderLinkDocumentation
