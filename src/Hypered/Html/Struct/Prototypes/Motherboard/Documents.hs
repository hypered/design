module Hypered.Html.Struct.Prototypes.Motherboard.Documents where

import Hypered.Html.Helpers
import Protolude hiding (div)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

--------------------------------------------------------------------------------
data Document = Document
  { documentId :: Text
  }

--------------------------------------------------------------------------------
prototypeMotherboardDocument :: Text -> Text -> Text -> Text -> Document -> Html
prototypeMotherboardDocument refliHomepage homepage justelUrl breadcrumb document@Document {..} = do
  H.docType
  H.html $ do -- TODO html(dir="ltr", lang="en")
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.meta ! A.name "viewport"
               ! A.content "width=device-width, initial-scale=1"
        H.link ! A.rel "stylesheet" ! A.href "/static/css/struct/foundations.css"
        H.link ! A.rel "stylesheet" ! A.href "/static/css/struct/ibm-plex.css"
        H.link ! A.rel "stylesheet" ! A.href "/static/css/struct/fonts.css"
        H.link ! A.rel "stylesheet" ! A.href "/static/css/struct/scale.css"
        H.link ! A.rel "stylesheet" ! A.href "/static/css/struct/layouts.css"
        H.link ! A.rel "stylesheet" ! A.href "/static/css/struct/misc.css"
    H.body ! A.class_ "u-container-vertical cover" $ do
        H.header $
            div "u-container" $
                div "c-text" $
                    H.div $
                        H.span ! A.class_ "logo" $
                            H.a ! A.href (H.toValue homepage) $ "Lex Iterata"

        div "u-container" $ do
            H.p $
                H.small ! A.class_ "breadcrumb" $ H.text breadcrumb

        H.footer $
          div "u-container" $ do
            H.hr
            div "c-text flow" $ do
              H.p $
                H.small $ do
                  "View this page in "
                  H.a ! A.href (H.toValue $ "/lex/" <> documentId) $ "JSON format"
                  "."
              H.p $
                H.small $ do
                  "View the "
                  H.a ! A.href (H.toValue $ justelUrl) $ "original page"
                  " on the Belgian Official Journal."
              H.p $
                H.small $ do
                  "Lex Iterata is a Refli experiment. "
                  H.a ! A.href (H.toValue homepage) $ "Read more"
                  " or go "
                  H.a ! A.href (H.toValue refliHomepage) $ "back to Refli"
                  "."
              H.p "© Hypered SRL, 2023."
