module Hypered.Html.Struct.Prototypes.Motherboard.Documents where

import qualified Data.Text as T
import Hypered.Html.Helpers
import Hypered.Html.Struct.Prototypes.Refli.Common as Struct
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
  , documentHasConcept :: Bool
  }

data Block =
    Pair Text Text
  | Indent Text
  | Table [[(Int, [Tag Text])]] -- TODO Remove dependency on TagSoup.
  | Monospace [Tag Text]

--------------------------------------------------------------------------------
prototypeMotherboardDocument :: Text -> Text -> Text -> Document -> Html
prototypeMotherboardDocument refliHomepage homepage breadcrumb Document {..} = do
  Struct.refliDocument
    Struct.defaultOptions
    "fr"
    documentFullTitle
    (documentFullTitle <> " Texte présenté par Lex Iterata, un projet pour faciliter la compréhension et l'analyse de textes législatifs belges.") $
    H.body ! A.class_ "u-container-vertical cover" $ do
        motherboardHeader homepage

        div "u-container" $ do
            H.p $
                H.small ! A.class_ "breadcrumb" $ H.text breadcrumb
            div "switcher switcher--bigger-left" $ do
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
                            when documentHasConcept $ do
                              H.dt $
                                H.dfn ! A.title "Ce texte est présent dans la base de connaissances de Refli" $ "Refli Concepts ✓"
                              H.dd $
                                H.a ! A.href "/en/concepts" $
                                  "Voir dans la base de connaissances"
                    mapM_ showBlock documentBlocks
                div "c-text flow-all" $
                  div "c-content center ad" $
                    H.p $
                      H.small $ do
                        H.text "Lex Iterata est un site web qui propose les textes législatifs consolidés du Moniteur Belge sous une nouvelle forme. Lex Iterata fait partie de "
                        H.a ! A.href "https://refli.be" $ H.text "Refli"
                        H.text ", qui vise à simplifier le calcul de salaire. Ces deux projets sont conçus par la société namuroise de développement informatique "
                        H.a ! A.href "https://hypered.be" $ H.text "Hypered"
                        H.text "."

        motherboardDocumentFooter documentId documentUrl homepage refliHomepage

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

--------------------------------------------------------------------------------
motherboardHeader :: Text -> Html
motherboardHeader homepage =
  H.header $
    div "u-container" $
      div "c-text" $
        H.div $
          H.span ! A.class_ "logo" $
            H.a ! A.href (H.toValue homepage) $ "Lex Iterata"

motherboardDocumentFooter :: Text -> Text -> Text -> Text -> Html
motherboardDocumentFooter documentId documentUrl homepage refliHomepage =
  H.footer $
    div "u-container" $ do
      H.hr
      div "c-text flow" $ do
        H.p $
          H.small $ do
            "Voir cette page au "
            H.a ! A.href (H.toValue $ "/lex/" <> documentId) $ "format JSON"
            "."
        H.p $
          H.small $ do
            "Voir la "
            H.a ! A.href (H.toValue $ documentUrl) $ "page originale"
            " au Moniteur Belge."
        H.p $
          H.small $ do
            "Lex Iterata est une expérience Refli. "
            H.a ! A.href (H.toValue homepage) $ "Lire au sujet de Lex Iterata"
            " ou "
            H.a ! A.href (H.toValue refliHomepage) $ "retourner vers Refli"
            "."
        H.p $
          H.small $ do
            "Lex Iterata et Refli sont des projets développés par "
            H.a ! A.href "https://hypered.be" $ "Hypered"
            "."
        H.p "© Hypered SRL, 2023-2024."
