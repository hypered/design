module Hypered.Html.Helpers where

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

div :: H.AttributeValue -> Html -> Html
div cls = H.div ! A.class_ cls
