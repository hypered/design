-- | Alternative to Hypered.Html.Tachyons, using the struct.css CSS instead.
module Hypered.Html.Struct.Specimens where

import Hypered.Html.Helpers
import Protolude hiding (div)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- Specimen: navigation.
-- See https://hypered.design/specimens/navigation.html.
specimenNavigation :: Html
specimenNavigation = do
  H.docType
  H.html $ do -- TODO html(dir="ltr", lang="en")
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.name "viewport"
             ! A.content "width=device-width, initial-scale=1.0"
      H.title "TODO" -- Not present in Pug.
      H.link ! A.rel "stylesheet" ! A.href "/static/css/struct.css"
    H.body ! A.class_ "u-container-vertical" $
      H.header $
        div "u-container" $
          div "u-bar" $ do
            div "u-bar__left" $
              H.a ! A.href "#" $
                H.img ! A.src "/static/images/logo.svg" ! A.alt "Refli"
            div "u-bar__right" $
              mainHeaderPageStruct

-- include ../includes/main-header--page-struct
mainHeaderPageStruct :: Html
mainHeaderPageStruct = H.p "TODO"
