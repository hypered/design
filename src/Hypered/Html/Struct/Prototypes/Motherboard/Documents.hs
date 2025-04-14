module Hypered.Html.Struct.Prototypes.Motherboard.Documents where

import qualified Data.Text as T
import Hypered.Html.Helpers
import Hypered.Html.Struct.Prototypes.Refli.Common as Struct
import Hypered.Html.Struct.Prototypes.Refli.LandingPage (prototypeRefliHeader, prototypeRefliMainNav', prototypeRefliFooter, refliMainHeaderTextsEn, refliMainHeaderTextsFr, refliMainHeaderTextsNl, refliNavigationBlockTextsEn, refliNavigationBlockTextsFr, refliNavigationBlockTextsNl)
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


data MotherboardDocumentTexts = MotherboardDocumentTexts
  { motherboardDocumentLanguage :: Text
  , motherboardDocumentTitlePresentation :: Text
  , motherboardDocumentSource :: Text
  , motherboardDocumentPublication :: Text
  , motherboardDocumentNumber :: Text
  , motherboardDocumentPage :: Text
  , motherboardDocumentOriginal :: Text
  , motherboardDocumentCase :: Text
  , motherboardDocumentStart :: Text
  , motherboardDocumentModify :: Text
  , motherboardDocumentXxx :: Text
  , motherboardDocumentConcept :: Text
  , motherboardDocumentConceptLink :: Text
  }

motherboardDocumentTextsEn :: MotherboardDocumentTexts
motherboardDocumentTextsEn = MotherboardDocumentTexts
  { motherboardDocumentLanguage = "en"
  , motherboardDocumentTitlePresentation =
      "Text presented by Lex Iterata, a project to facilitate the understanding and analysis of Belgian legislative texts."
  , motherboardDocumentSource = "Source"
  , motherboardDocumentPublication = "Publication"
  , motherboardDocumentNumber = "Number"
  , motherboardDocumentPage = "Page"
  , motherboardDocumentOriginal = "original version"
  , motherboardDocumentCase = "Case number"
  , motherboardDocumentStart = "Entry into force / Effect"
  , motherboardDocumentModify = "Modified text"
  , motherboardDocumentXxx = "Council of State"
  , motherboardDocumentConcept =
      "This text is present in the Refli knowledge base"
  , motherboardDocumentConceptLink = "View in knowledge base"
  }

motherboardDocumentTextsFr :: MotherboardDocumentTexts
motherboardDocumentTextsFr = MotherboardDocumentTexts
  { motherboardDocumentLanguage = "fr"
  , motherboardDocumentTitlePresentation =
      "Texte présenté par Lex Iterata, un projet pour faciliter la compréhension et l'analyse de textes législatifs belges."
  , motherboardDocumentSource = "Source"
  , motherboardDocumentPublication = "Publication"
  , motherboardDocumentNumber = "Numéro"
  , motherboardDocumentPage = "Page"
  , motherboardDocumentOriginal = "version originale"
  , motherboardDocumentCase = "Dossier numéro"
  , motherboardDocumentStart = "Entrée en vigueur / Effet"
  , motherboardDocumentModify = "Texte modifié"
  , motherboardDocumentXxx = "Conseil d'Etat"
  , motherboardDocumentConcept =
      "Ce texte est présent dans la base de connaissances de Refli"
  , motherboardDocumentConceptLink = "Voir dans la base de connaissances"
  }

motherboardDocumentTextsNl :: MotherboardDocumentTexts
motherboardDocumentTextsNl = MotherboardDocumentTexts
  { motherboardDocumentLanguage = "nl"
  , motherboardDocumentTitlePresentation =
      "Tekst gepresenteerd door Lex Iterata, een project om het begrip en de analyse van Belgische wetgevende teksten te vergemakkelijken."
  , motherboardDocumentSource = "Bron"
  , motherboardDocumentPublication = "Publicatie"
  , motherboardDocumentNumber = "Nummer"
  , motherboardDocumentPage = "Pagina"
  , motherboardDocumentOriginal = "originele versie"
  , motherboardDocumentCase = "Dossiernummer"
  , motherboardDocumentStart = "Inwerkingtreding / Effect"
  , motherboardDocumentModify = "Gewijzigde tekst"
  , motherboardDocumentXxx = "Raad van State"
  , motherboardDocumentConcept =
      "Deze tekst is aanwezig in de kennisbank van Refli"
  , motherboardDocumentConceptLink = "Bekijk in kennisbank"
  }

--------------------------------------------------------------------------------
data MotherboardDocumentFooterTexts = MotherboardDocumentFooterTexts
  { motherboardDocumentFooterLanguage :: Text
  , motherboardDocumentFooterSeeJson :: Text
  , motherboardDocumentFooterJsonFormat :: Text
  , motherboardDocumentFooterSeeOriginal :: Text
  , motherboardDocumentFooterOriginalPage :: Text
  , motherboardDocumentFooterJournal :: Text
  }

motherboardDocumentFooterTextsEn :: MotherboardDocumentFooterTexts
motherboardDocumentFooterTextsEn = MotherboardDocumentFooterTexts
  { motherboardDocumentFooterLanguage = "en"
  , motherboardDocumentFooterSeeJson = "View this page in"
  , motherboardDocumentFooterJsonFormat = "JSON format"
  , motherboardDocumentFooterSeeOriginal = "View the"
  , motherboardDocumentFooterOriginalPage = "original page"
  , motherboardDocumentFooterJournal = "in the Belgian Official Journal."
  }

motherboardDocumentFooterTextsFr :: MotherboardDocumentFooterTexts
motherboardDocumentFooterTextsFr = MotherboardDocumentFooterTexts
  { motherboardDocumentFooterLanguage = "fr"
  , motherboardDocumentFooterSeeJson = "Voir cette page au"
  , motherboardDocumentFooterJsonFormat = "format JSON"
  , motherboardDocumentFooterSeeOriginal = "Voir la"
  , motherboardDocumentFooterOriginalPage = "page originale"
  , motherboardDocumentFooterJournal = "au Moniteur Belge."
  }

motherboardDocumentFooterTextsNl :: MotherboardDocumentFooterTexts
motherboardDocumentFooterTextsNl = MotherboardDocumentFooterTexts
  { motherboardDocumentFooterLanguage = "nl"
  , motherboardDocumentFooterSeeJson = "Bekijk deze pagina in"
  , motherboardDocumentFooterJsonFormat = "JSON-formaat"
  , motherboardDocumentFooterSeeOriginal = "Bekijk de"
  , motherboardDocumentFooterOriginalPage = "originele pagina"
  , motherboardDocumentFooterJournal = "in het Belgisch Staatsblad."
  }

--------------------------------------------------------------------------------
prototypeMotherboardDocument :: Text -> Text -> Text -> Text -> Document -> Html
prototypeMotherboardDocument lang refliHomepage homepage breadcrumb Document {..} = do
  let url = "/lex/" <> documentId
      MotherboardDocumentTexts {..} = case lang of
        "en" -> motherboardDocumentTextsEn
        "nl" -> motherboardDocumentTextsNl
        _ -> motherboardDocumentTextsFr
  Struct.refliDocument'
    Struct.defaultOptions
    lang
    (Just url)
    documentFullTitle
    (documentFullTitle <> " " <> motherboardDocumentTitlePresentation ) $
    H.body ! A.class_ "u-container-vertical cover" $ do
        motherboardHeader lang homepage

        div "u-container" $ do
            div "flow-all" $ do
              H.h3 "Lex Iterata"
              H.small ! A.class_ "breadcrumb" $ H.text breadcrumb
            div "c-text flow-all limit-42em legislation" $ do
                    H.h1 ! A.class_ "mb-title" $ H.text documentFullTitle

                    H.div $
                        H.dl ! A.class_ "mb-pairs" $ do
                            H.dt "ELI"
                            H.dd $
                              H.a ! A.href (H.toValue documentUrl) $ "Justel"
                            H.dt $ H.text motherboardDocumentSource
                            H.dd $ H.text documentSource
                            H.dt $ H.text motherboardDocumentPublication
                            H.dd $ H.text documentPublicationDate
                            H.dt $ H.text motherboardDocumentNumber
                            H.dd $ H.text documentNumber
                            H.dt $ H.text motherboardDocumentPage
                            H.dd $ H.text $ show documentPageNumber
                            H.dt "PDF"
                            H.dd $
                              maybe
                                (H.text motherboardDocumentOriginal)
                                (\lnk -> H.a ! A.href (H.toValue $
                                  "https://www.ejustice.just.fgov.be" <> lnk) $
                                    H.text motherboardDocumentOriginal)
                                documentPDFOriginal
                            H.dt $ H.text motherboardDocumentCase
                            H.dd $ H.text documentCaseNumber
                            H.dt $ H.text motherboardDocumentStart
                            H.dd $
                              mapM_ (\(a, b) -> H.text a >> H.text b) documentStartDates
                            H.dt $ H.text motherboardDocumentModify
                            H.dd $
                              mapM_ (\a -> H.text a) documentModifies
                            H.dt "belgiquelex"
                            H.dd $ do
                              let f lnk =
                                    if "http://reflex.raadvst-consetat.be" `T.isPrefixOf` lnk
                                    then motherboardDocumentXxx
                                    else "TODO"
                              mapM_
                                (\lnk -> H.a ! A.href (H.toValue lnk) $ H.text (f lnk))
                                documentLegislativeLinks
                            when documentHasConcept $ do
                              H.dt $
                                H.dfn ! A.title (H.toValue motherboardDocumentConcept) $ "Refli Concepts ✓"
                              H.dd $
                                H.a ! A.href "/en/concepts" $
                                  H.text motherboardDocumentConceptLink
                    mapM_ showBlock documentBlocks

        motherboardDocumentFooter lang documentId documentUrl homepage refliHomepage

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
motherboardHeader :: Text -> Text -> Html
motherboardHeader lang homepage =
  prototypeRefliHeader
    lang
    ( prototypeRefliMainNav' $
        case lang of
          "en" -> refliMainHeaderTextsEn
          "nl" -> refliMainHeaderTextsNl
          _ -> refliMainHeaderTextsFr
    )

motherboardDocumentFooter :: Text -> Text -> Text -> Text -> Text -> Html
motherboardDocumentFooter lang documentId documentUrl homepage refliHomepage = do
  let MotherboardDocumentFooterTexts {..} = case lang of
        "en" -> motherboardDocumentFooterTextsEn
        "nl" -> motherboardDocumentFooterTextsNl
        _ -> motherboardDocumentFooterTextsFr
      nbTexts = case lang of
        "en" -> refliNavigationBlockTextsEn
        "nl" -> refliNavigationBlockTextsNl
        _ -> refliNavigationBlockTextsFr
      url = "/lex/" <> documentId
  H.footer $
    div "u-container" $ do
      H.hr
      div "c-text flow" $ do
        H.p $
          H.small $ do
            H.text $ motherboardDocumentFooterSeeJson <> " "
            H.a ! A.href (H.toValue url) $
              H.text motherboardDocumentFooterJsonFormat
            "."
        H.p $
          H.small $ do
            H.text $ motherboardDocumentFooterSeeOriginal <> " "
            H.a ! A.href (H.toValue $ documentUrl) $
              H.text motherboardDocumentFooterOriginalPage
            H.text $ " " <> motherboardDocumentFooterJournal

  prototypeRefliFooter lang url nbTexts
