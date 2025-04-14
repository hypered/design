module Hypered.Html.Struct.Prototypes.Refli.Common where

import Hypered.Html.Common (
  autoReload,
 )
import Protolude hiding (div)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

--------------------------------------------------------------------------------
data RefliDocumentOptions = RefliDocumentOptions
  { refliDocumentAutoreload :: Bool -- ^ Whether to inject the auto-reloading JavaScript (websocket-based) code to refresh the page during development.
  , refliDocumentHtmx :: Bool -- ^ Whether to reference the htmx JavaScript library.
  }

defaultOptions :: RefliDocumentOptions
defaultOptions = RefliDocumentOptions
  { refliDocumentAutoreload = False
  , refliDocumentHtmx = False
  }

--------------------------------------------------------------------------------
refliDocument :: RefliDocumentOptions -> Text -> Text -> Text -> Html -> Html
refliDocument opts lang title description body =
  refliDocument' opts lang Nothing title description body

refliDocument' :: RefliDocumentOptions -> Text -> Maybe Text -> Text -> Text -> Html -> Html
refliDocument' opts lang murl title description body = do
  H.docType
  H.html ! A.dir "ltr" ! A.lang (H.toValue lang) $ do
    refliHead' opts murl title description
    body

--------------------------------------------------------------------------------
refliHead :: RefliDocumentOptions -> Text -> Text -> Html
refliHead opts title description = refliHead' opts Nothing title description

-- murl looks like /lex/xxx. When provided, /fr/lex/xxx is set as the canonical
-- page and alternate tags are set too.
refliHead' :: RefliDocumentOptions -> Maybe Text -> Text -> Text -> Html
refliHead' RefliDocumentOptions {..} murl title description =
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport"
           ! A.content "width=device-width, initial-scale=1"
    H.link ! A.rel "stylesheet" ! A.href "/static/css/struct.min.css"
    when refliDocumentHtmx $
      H.script ! A.src "/static/js/htmx-1.9.12.min.js" $ mempty
    H.link ! A.rel "preload" ! A.href "/static/fonts/IBMPlexSans-Regular.woff2"
           ! H.customAttribute "as" "font" ! A.type_ "font/woff2"
           ! H.customAttribute "crossorigin" "crossorigin"
    H.link ! A.rel "preload" ! A.href "/static/fonts/IBMPlexSans-Medium.woff2"
           ! H.customAttribute "as" "font" ! A.type_ "font/woff2"
           ! H.customAttribute "crossorigin" "crossorigin"
    H.link ! A.rel "preload" ! A.href "/static/fonts/IBMPlexSans-SemiBold.woff2"
           ! H.customAttribute "as" "font" ! A.type_ "font/woff2"
           ! H.customAttribute "crossorigin" "crossorigin"
    H.link ! A.rel "preload" ! A.href "/static/fonts/IBMPlexSans-Bold.woff2"
           ! H.customAttribute "as" "font" ! A.type_ "font/woff2"
           ! H.customAttribute "crossorigin" "crossorigin"
    H.meta ! A.name "description"
           ! A.content (H.toValue description)
    case murl of
      Nothing -> pure ()
      Just url -> do
       H.link ! A.rel "canonical"
              ! A.href (H.toValue $ "https://refli.be/fr" <> url)
       H.link ! A.rel "alternate"
              ! A.hreflang "en"
              ! A.href (H.toValue $ "https://refli.be/en" <> url)
       H.link ! A.rel "alternate"
              ! A.hreflang "fr"
              ! A.href (H.toValue $ "https://refli.be/fr" <> url)
       H.link ! A.rel "alternate"
              ! A.hreflang "nl"
              ! A.href (H.toValue $ "https://refli.be/nl" <> url)
    H.title $ H.text title
    when refliDocumentAutoreload autoReload
