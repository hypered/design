module Hypered.Html.Struct.Prototypes.Motherboard.Documents where

import qualified Data.Text as T
import Hypered.Html.Helpers
import Protolude hiding (div)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.HTML.TagSoup (innerText, Tag(..), (~==))

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
  , documentBlocks :: [Block]
    -- ^ TODO Probably change this to Html so there is a bit more control
    -- offered to the caller.
  }

data Block =
    Pair Text Text
  | Indent Text
  | Table [[(Int, [Tag Text])]] -- TODO Remove dependency on TagSoup.
  | Monospace [Tag Text]

--------------------------------------------------------------------------------
prototypeMotherboardDocument :: Text -> Text -> Text -> Document -> Html
prototypeMotherboardDocument refliHomepage homepage breadcrumb Document {..} = do
  H.docType
  -- TODO Actually write the page in French.
  H.html ! A.dir "ltr" ! A.lang "fr" $ do
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
               ! A.content (H.toValue $ documentFullTitle <> " Texte présenté par Lex Iterata, un projet pour faciliter la compréhension et l'analyse de textes législatifs belges.")
        H.title "Refli - Lex Iterata"
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
                  H.a ! A.href (H.toValue homepage) $ "Read more about Lex Iterata"
                  " or go "
                  H.a ! A.href (H.toValue refliHomepage) $ "back to Refli"
                  "."
              H.p "© Hypered SRL, 2023-2024."

showBlock :: Block -> Html
showBlock (Pair level content) =
  H.p $ do
    H.span ! A.class_ "article" $ H.text level
    H.text content

showBlock (Indent content) =
  H.p $ do
    H.text content

showBlock (Table xs) = formatTable xs

showBlock (Monospace xs) = formatMonospace xs

formatTable :: [[(Int, [Tag Text])]] -> Html
formatTable [] = H.table mempty
formatTable xs = H.table $
  mapM_ formatRow xs

formatRow :: [(Int, [Tag Text])] -> Html
formatRow cs = H.tr $ mapM_ formatCell cs

formatCell :: (Int, [Tag Text]) -> Html
formatCell (1, ts) = H.td . H.text $ innerText ts
formatCell (n, ts) = H.td ! A.colspan (H.toValue @Text $ show n) $ H.text $ innerText ts

formatMonospace :: [Tag Text] -> Html
formatMonospace xs =
  H.pre $
    H.code $
      mapM_ (H.text . f) xs'
 where
  xs' = dropWhile (~== (TagOpen @Text "br" [])) xs
  f (TagOpen "a" _) = ""
  f (TagClose "a") = ""
  f (TagOpen "b" _) = ""
  f (TagClose "b") = ""
  f (TagOpen "br" _) = "\n"
  f (TagOpen "font" _) = ""
  f (TagClose "font") = ""
  f (TagOpen "sup" _) = ""
  f (TagClose "sup") = ""
  -- The th tr table are specifically for
  -- https://www.ejustice.just.fgov.be/eli/arrete/1996/01/31/1996035268/justel
  -- where the </font> tag is missing.
  f (TagClose "th") = ""
  f (TagClose "tr") = ""
  f (TagClose "table") = ""
  f (TagText s) = T.map g s
  f x = panic $ "formatMonospace: unexpected value: " <> show x
  g '\160' = ' '
  g c = c
