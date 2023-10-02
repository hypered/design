module Hypered.Html.Struct.Prototypes.Motherboard.Documents where

import qualified Data.Text as T
import Hypered.Html.Helpers
import Protolude hiding (div)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

--------------------------------------------------------------------------------
data Document = Document
  { documentId :: Text
  , documentUrl :: Text
  , documentFullTitle :: Text
  , documentSource :: Text
  , documentPublicationDate :: Text
  , documentNumber :: Text
  , documentPageNumber :: Int
  , documentPDFOriginal :: Maybe Text
  , documentCaseNumber :: Text
  , documentStartDates :: [(Text, Text)]
  , documentModifies :: [Text]
  , documentLegislativeLinks :: [Text]
  , documentBlocks :: [(Text, Text)]
    -- ^ TODO Probably change this to Html so there is a bit more control
    -- offered to the caller.
  }

--------------------------------------------------------------------------------
prototypeMotherboardDocument :: Text -> Text -> Text -> Document -> Html
prototypeMotherboardDocument refliHomepage homepage breadcrumb Document {..} = do
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
            div "c-text flow-all limit-42em legislation" $ do
                H.h1 ! A.class_ "mb-title" $ H.text documentFullTitle

                H.div $
                    H.dl ! A.class_ "mb-pairs" $ do
                        H.dt "ELI"
                        H.dd $
                          H.a ! A.href (H.toValue documentUrl) $ "Justel"
                        H.dt "Source"
                        H.dd $ H.text documentSource
                        H.dt "Publication"
                        H.dd $ H.text documentPublicationDate
                        H.dt "Numéro"
                        H.dd $ H.text documentNumber
                        H.dt "Page"
                        H.dd $ H.text $ show documentPageNumber
                        H.dt "PDF"
                        H.dd $
                          maybe
                            "verion originale"
                            (\lnk -> H.a ! A.href (H.toValue $
                              "https://www.ejustice.just.fgov.be" <> lnk) $
                                "version originale")
                            documentPDFOriginal
                        H.dt "Dossier numéro"
                        H.dd $ H.text documentCaseNumber
                        H.dt "Entrée en vigueur / Effet"
                        H.dd $
                          mapM_ (\(a, b) -> H.text a >> H.text b) documentStartDates
                        H.dt "Texte modifié"
                        H.dd $
                          mapM_ (\a -> H.text a) documentModifies
                        H.dt "belgiquelex"
                        H.dd $ do
                          let f lnk =
                                if "http://reflex.raadvst-consetat.be" `T.isPrefixOf` lnk
                                then "Conseil d'Etat"
                                else "TODO"
                          mapM_
                            (\lnk -> H.a ! A.href (H.toValue lnk) $ H.text (f lnk))
                            documentLegislativeLinks
                mapM_ showBlock documentBlocks

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
                  H.a ! A.href (H.toValue $ documentUrl) $ "original page"
                  " on the Belgian Official Journal."
              H.p $
                H.small $ do
                  "Lex Iterata is a Refli experiment. "
                  H.a ! A.href (H.toValue homepage) $ "Read more"
                  " or go "
                  H.a ! A.href (H.toValue refliHomepage) $ "back to Refli"
                  "."
              H.p "© Hypered SRL, 2023."

showBlock (level, content) =
  H.p $ do
      H.span ! A.class_ "article" $ H.text level
      H.text content
