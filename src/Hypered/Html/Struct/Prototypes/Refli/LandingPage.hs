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

prototypeRefliLandingPage :: Bool -> MainHeaderTexts -> LandingPageTexts -> Html
prototypeRefliLandingPage autoreload mhTexts texts@LandingPageTexts {..} = do
  refliDocument
    autoreload landingPageLanguage landingPageTitle landingPageDescription $
      prototypeRefliPage
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

prototypeRefliPage :: Html -> Html -> Html
prototypeRefliPage header content =
  H.body ! A.class_ "u-container-vertical" $ do
    H.header $
      div "u-container" $
        div "u-bar" $ do
          div "u-bar__left" $
            H.a ! A.href "/prototypes/refli/index-constant.html" $
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
                H.a ! A.href "/en" $ "EN" -- TODO append the rest of the URL.
              H.li $
                H.a ! A.href "/fr" $ "FR"
              H.li $
                H.a ! A.href "/nl" $ "NL"

        div "flow u-flow-c-4" $
          H.span "© Hypered SRL, 2023-2024."

prototypeRefliMainHeader :: MainHeaderTexts -> Html
prototypeRefliMainHeader MainHeaderTexts {..} = do
  let linkBlog = mainHeaderLanguage <> "/blog"
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
          H.a ! A.href "/prototypes/refli/describe.html" $
            H.text mainHeaderLinkComputeSalaries

    H.li $
      div "menu-item" $
        H.a ! A.href "#" ! A.class_ "menu-link" $ H.text mainHeaderLinkDocumentation
