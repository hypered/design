-- | Alternative to Hypered.Html.Tachyons, using the struct.css CSS instead.
module Hypered.Html.Struct.Specimens where

import Hypered.Html.Helpers
import Hypered.Html.Struct.Icons (arrowRight)
import Protolude hiding (div)
import Text.Blaze (customAttribute)
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

-- Specimen: invoke (form), and invoke-result
-- See https://hypered.design/specimens/invoke--form.html and
-- https://hypered.design/specimens/invoke-result.html.
specimenInvokeForm :: Text -> Text -> Html
specimenInvokeForm input output = do
  H.docType
  H.html ! A.dir "ltr" ! A.lang "en" $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.name "viewport"
             ! A.content "width=device-width, initial-scale=1.0"
      H.title "The Invoke (form)"
      H.link ! A.rel "stylesheet" ! A.href "/static/css/struct.css"
      H.script ! A.src "/static/js/htmx-1.9.12.min.js" $ mempty
    H.body $ do
      div "u-container" $ do
        div "c-text flow-all" $ do
          H.h1 "The Invoke (form)"
          H.p "The Invoke is a two-column layout where the left part is some kind of input (e.g. a text editor, some UI widgets, a form, ...) and the right part shows some result."
      div "u-container" $
        div "switcher" $ do
          H.form ! A.action "/specimens/invoke-result"
                 ! A.method "POST"
                 ! A.class_ "c-content flow" $ do
            H.input ! A.class_ "no-style"
                    ! A.type_ "text"
                    ! A.id "input-text"
                    ! A.name "input-text"
                    ! A.value (H.toValue input)
            H.div $
              H.button ! A.class_ "c-button c-button--primary" $ do
                H.span "Process"
                arrowRight
            H.div $
              H.button ! A.class_ "c-button c-button--secondary" $
                H.span "Reset"
          div "flow" $
            H.p ! A.id "output-text" $ H.text output

specimenInvokeHtmx :: Text -> Text -> Html
specimenInvokeHtmx input output = do
  H.docType
  H.html ! A.dir "ltr" ! A.lang "en" $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.name "viewport"
             ! A.content "width=device-width, initial-scale=1.0"
      H.title "The Invoke (form)"
      H.link ! A.rel "stylesheet" ! A.href "/static/css/struct.css"
      H.script ! A.src "/static/js/htmx-1.9.12.min.js" $ mempty
    H.body $ do
      div "u-container" $ do
        div "c-text flow-all" $ do
          H.h1 "The Invoke (form)"
          H.p "The Invoke is a two-column layout where the left part is some kind of input (e.g. a text editor, some UI widgets, a form, ...) and the right part shows some result."
      div "u-container" $
        div "switcher" $ do
          H.form ! A.action "/specimens/invoke-result"
                 ! A.method "POST"
                 ! customAttribute "hx-post" "/specimens/invoke-result"
                 ! customAttribute "hx-swap" "outerHTML"
                 ! customAttribute "hx-target" "#output-text"
                 ! customAttribute "hx-select" "#output-text"
                 ! A.class_ "c-content flow" $ do
            H.input ! A.class_ "no-style"
                    ! A.type_ "text"
                    ! A.id "input-text"
                    ! A.name "input-text"
                    ! A.value (H.toValue input)
            H.div $
              H.button ! A.class_ "c-button c-button--primary" $ do
                H.span "Process"
                arrowRight
            H.div $
              H.button ! A.class_ "c-button c-button--secondary" $
                H.span "Reset"
          div "flow" $
            H.p ! A.id "output-text" $ H.text output
