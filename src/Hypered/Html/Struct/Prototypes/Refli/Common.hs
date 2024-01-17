module Hypered.Html.Struct.Prototypes.Refli.Common where

import qualified Data.Text as T
import Hypered.Html.Common (
  autoReload,
 )
import Hypered.Html.Helpers
import Protolude hiding (div)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.HTML.TagSoup (innerText, Tag(..), (~==))

--------------------------------------------------------------------------------
refliDocument :: Bool -> Text -> Text -> Text -> Html -> Html
refliDocument autoreload lang title description body = do
  H.docType
  H.html ! A.dir "ltr" ! A.lang (H.toValue lang) $ do
    refliHead autoreload title description
    body

--------------------------------------------------------------------------------
refliHead :: Bool -> Text -> Text -> Html
refliHead autoreload title description =
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport"
           ! A.content "width=device-width, initial-scale=1"
    H.link ! A.rel "stylesheet" ! A.href "/static/css/struct.min.css"
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
    H.title $ H.text title
    when autoreload autoReload
