module Hypered.Html.Struct.Prototypes.Motherboard.Indices where

import Hypered.Html.Helpers
import Hypered.Html.Struct.Prototypes.Motherboard.Documents (motherboardDocumentFooterTextsEn, motherboardDocumentFooterTextsFr, motherboardDocumentFooterTextsNl, MotherboardDocumentFooterTexts(..))
import Hypered.Html.Struct.Prototypes.Refli.Common (defaultOptions, refliHead')
import Hypered.Html.Struct.Prototypes.Refli.LandingPage (prototypeRefliHeader, prototypeRefliMainNav', prototypeRefliFooter, refliMainHeaderTextsFr, refliNavigationBlockTextsFr, MainHeaderTexts(..), NavigationBlockTexts(..))
import Protolude hiding (div)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

--------------------------------------------------------------------------------
data MotherboardHomepageTexts = MotherboardHomepageTexts
  { motherboardHomepageLanguage :: Text
  , motherboardHomepageDescription :: Text
  , motherboardHomepageParagraph1 :: Text
  , motherboardHomepageParagraph2 :: Text
  , motherboardHomepageParagraph3 :: Text
  , motherboardHomepageWord1 :: Text
  , motherboardHomepageWord2 :: Text
  , motherboardHomepageWord3 :: Text
  , motherboardHomepageWord4 :: Text
  , motherboardHomepageWord5 :: Text
  }

-- Hard-coded for now. We should probably use the PO-based system from Refli.
motherboardHomepageTextsEn :: MotherboardHomepageTexts
motherboardHomepageTextsEn = MotherboardHomepageTexts
  { motherboardHomepageLanguage = "en"
  , motherboardHomepageDescription = ""
  , motherboardHomepageParagraph1 =
      "Lex Iterata collects and transforms the content of the \
      \<a href=\"https://www.ejustice.just.fgov.be/\">Belgian Official Journal</a>\
      \, presenting it in an enhanced format for human comprehension, \
      \while also providing structured data tailored for machine consumption."
  , motherboardHomepageParagraph2 =
      "Lex Iterata is continually evolving. At present, we provide \
      \access to collections of documents under the \
      \<a href=\"https://www.ejustice.just.fgov.be/eli/\">ELI (European \
      \Legislation Identifier) scheme</a>\
      \. Both user-friendly web pages and structured JSON indices \
      \are available for your convenience."
  , motherboardHomepageParagraph3 =
      "The ELI framework as used in Belgium encompasses five distinct \
      \document categories:"
  , motherboardHomepageWord1 = "Constitution"
  , motherboardHomepageWord2 = "Law"
  , motherboardHomepageWord3 = "Decree"
  , motherboardHomepageWord4 = "Ordinance"
  , motherboardHomepageWord5 = "Order"
  }

motherboardHomepageTextsFr :: MotherboardHomepageTexts
motherboardHomepageTextsFr = MotherboardHomepageTexts
  { motherboardHomepageLanguage = "fr"
  , motherboardHomepageDescription = ""
  , motherboardHomepageParagraph1 =
      "Lex Iterata collecte et transforme le contenu du \
      \<a href=\"https://www.ejustice.just.fgov.be/\">Moniteur Belge</a>, \
      \en le présentant dans un format amélioré pour la compréhension humaine, \
      \tout en fournissant des données structurées adaptées au traitement \
      \informatique."
  , motherboardHomepageParagraph2 =
      "Lex Iterata est en constante évolution. Actuellement, nous donnons \
      \accès à des collections de documents du système <a href=\
      \\"https://www.ejustice.just.fgov.be/eli/\">ELI (European \
      \Legislation Identifier)</a>. Aussi bien des pages web conviviales que \
      \des index JSON structurés sont disponibles."
  , motherboardHomepageParagraph3 =
      "Le cadre ELI tel qu'il est utilisé en Belgique comprend cinq catégories \
      \de documents distinctes:"
  , motherboardHomepageWord1 = "Constitution"
  , motherboardHomepageWord2 = "Loi"
  , motherboardHomepageWord3 = "Décret"
  , motherboardHomepageWord4 = "Ordonnance"
  , motherboardHomepageWord5 = "Arrêté"
  }

motherboardHomepageTextsNl :: MotherboardHomepageTexts
motherboardHomepageTextsNl = MotherboardHomepageTexts
  { motherboardHomepageLanguage = "nl"
  , motherboardHomepageDescription = ""
  , motherboardHomepageParagraph1 =
      "Lex Iterata verzamelt en transformeert de inhoud van het \
      \<a href=\"https://www.ejustice.just.fgov.be/\">Belgisch Staatsblad</a>, \
      \en presenteert deze in een verbeterd formaat voor menselijk begrip, \
      \terwijl ook gestructureerde gegevens worden aangeboden die geschikt zijn \
      \voor machinale verwerking."
  , motherboardHomepageParagraph2 =
      "Lex Iterata evolueert voortdurend. Momenteel bieden we \
      \toegang tot verzamelingen documenten onder het \
      \<a href=\"https://www.ejustice.just.fgov.be/eli/\">ELI (European \
      \Legislation Identifier) schema</a>. \
      \Zowel gebruiksvriendelijke webpagina's als gestructureerde JSON-indexen \
      \zijn beschikbaar voor uw gemak."
  , motherboardHomepageParagraph3 =
      "Het ELI-kader zoals gebruikt in België omvat vijf verschillende \
      \documentcategorieën:"
  , motherboardHomepageWord1 = "Grondwet"
  , motherboardHomepageWord2 = "Wet"
  , motherboardHomepageWord3 = "Decreet"
  , motherboardHomepageWord4 = "Ordonnantie"
  , motherboardHomepageWord5 = "Besluit"
  }

--------------------------------------------------------------------------------
-- Prototype: motherboard-index.
-- See http://127.0.0.1:3002/prototypes/refli/motherboard-index.html.
prototypeMotherboardHomepage :: Text -> MainHeaderTexts -> MotherboardHomepageTexts -> NavigationBlockTexts -> Text -> Text -> Html
prototypeMotherboardHomepage lang mhTexts texts nbTexts refliHomepage homepage = do
  let MotherboardHomepageTexts {..} = texts
  H.docType
  H.html ! A.dir "ltr" ! A.lang (H.toValue motherboardHomepageLanguage) $ do
    refliHead' defaultOptions (Just "/lex") "Lex Iterata" ""
    H.body ! A.class_ "u-container-vertical cover" $ do
      prototypeRefliHeader lang (prototypeRefliMainNav' mhTexts)

      div "u-container" $ do
        div "c-content flow-all limit-42em" $
          H.h1 "Lex Iterata"
        div "switcher" $ do
          div "flow-all" $
            H.p $ H.preEscapedText motherboardHomepageParagraph1

          H.div $ pure ()

        div "c-text flow-all limit-42em" $ do
          H.p $ H.preEscapedText motherboardHomepageParagraph2
          H.p $ H.preEscapedText motherboardHomepageParagraph3
          H.ul $ do
            H.li $
              H.a ! A.href (H.toValue $ "/" <> lang <> "/lex/constitution/1994") $ H.text motherboardHomepageWord1
            H.li $
              H.a ! A.href (H.toValue $ "/" <> lang <> "/lex/law/2025") $ H.text motherboardHomepageWord2
            H.li $
              H.a ! A.href (H.toValue $ "/" <> lang <> "/lex/decree/2025") $ H.text motherboardHomepageWord3
            H.li $
              H.a ! A.href (H.toValue $ "/" <> lang <> "/lex/ordinance/2025") $ H.text motherboardHomepageWord4
            H.li $
              H.a ! A.href (H.toValue $ "/" <> lang <> "/lex/order/2025") $ H.text motherboardHomepageWord5

      prototypeRefliFooter lang "/lex" nbTexts

--------------------------------------------------------------------------------
-- Prototype: motherboard-index-1.
-- See https://hypered.design/prototypes/refli/motherboard-index-1.html.
prototypeMotherboardIndex1 :: Html
prototypeMotherboardIndex1 =
  prototypeMotherboardIndex
    "fr"
    refliNavigationBlockTextsFr
    refliMainHeaderTextsFr
    "/specimens/navigation"
    "/lex" "constitution/1831" "constitution/1831" False "Constitution / 1831" [entry]
    [1000, 2000] -- TODO Better example years
 where
  entry = Entry {..}
  entryId = "1831020701"
  entryTitle = "7 FEVRIER 1831. - CONSTITUTION DE LA BELGIQUE."
  entrySource = ""
  entryPublicationDate = "07-02-1831"
  entryJournalLink = Nothing
  entryJustelLink =
    "https://www.ejustice.just.fgov.be/eli/constitution/1831/02/07/1831020701/justel"

--------------------------------------------------------------------------------
-- Prototype: motherboard-index-dense.
-- See https://hypered.design/prototypes/refli/motherboard-index-dense.html.
prototypeMotherboardIndexDense :: Html
prototypeMotherboardIndexDense =
  prototypeMotherboardIndex
    "fr"
    refliNavigationBlockTextsFr
    refliMainHeaderTextsFr
    "/specimens/navigation"
    "/lex" "law/2022" "loi/2022" True "Loi / 2022" [entry1, entry2, entry3, entry4]
    [1000, 2000] -- TODO Better example years
 where
  entry1 = Entry
    { entryId = "2022030600"
    , entryTitle = "19 JANVIER 2022. - Loi portant le livre 2, titre 3, \"Les relations patrimoniales des couples\" et le livre 4 \"Les successions, donations et testaments\" du Code civil (1)"
    , entryPublicationDate = "14-03-2022"
    , entrySource = "SERVICE PUBLIC FEDERAL JUSTICE"
    , entryJournalLink = Just
        "https://www.ejustice.just.fgov.be/eli/loi/2022/01/19/2022030600/moniteur"
    , entryJustelLink =
        "https://www.ejustice.just.fgov.be/eli/loi/2022/01/19/2022030600/justel"
    }
  entry2 = Entry
    { entryId = "2022A30600"
    , entryTitle = "19 JANVIER 2022. - CODE CIVIL - LIVRE 2, Titre 3 : \" Les relations patrimoniales des couples \""
    , entryPublicationDate = "14-03-2022"
    , entrySource = "JUSTICE"
    , entryJournalLink = Nothing
    , entryJustelLink =
        "https://www.ejustice.just.fgov.be/eli/loi/2022/01/19/2022A30600/justel"
    }
  entry3 = Entry
    { entryId = "2022B30600"
    , entryTitle = "19 JANVIER 2022. - CODE CIVIL - LIVRE 4 : \" Les successions, donations et testaments \""
    , entryPublicationDate = "14-03-2022"
    , entrySource = "JUSTICE"
    , entryJournalLink = Nothing
    , entryJustelLink =
        "https://www.ejustice.just.fgov.be/eli/loi/2022/01/19/2022B30600/justel"
    }
  entry4 = Entry
    { entryId = "2022040046"
    , entryTitle = "21 JANVIER 2022. - Loi portant des dispositions fiscales diverses (1)"
    , entryPublicationDate = "28-01-2022"
    , entrySource = "SERVICE PUBLIC FEDERAL FINANCES"
    , entryJournalLink = Just
        "http://www.ejustice.just.fgov.be/eli/loi/2022/01/21/2022040046/moniteur"
    , entryJustelLink =
        "http://www.ejustice.just.fgov.be/eli/loi/2022/01/21/2022040046/justel"
    }

data Entry = Entry
  { entryId :: Text
  , entryTitle :: Text
  , entryPublicationDate :: Text
  , entrySource :: Text
  , entryJournalLink :: Maybe Text
  , entryJustelLink :: Text
  }

--------------------------------------------------------------------------------
prototypeMotherboardIndex :: Text -> NavigationBlockTexts -> MainHeaderTexts -> Text -> Text -> Text -> Text -> Bool -> Text -> [Entry] -> [Int] -> Html
prototypeMotherboardIndex lang nbTexts mhTexts refliHomepage homepage fragment fragmentFr dense breadcrumb entries years = do
  let url = "/lex/" <> fragment
      entries' = zip [(1 :: Int) ..] entries
  H.docType
  H.html ! A.dir "ltr" ! A.lang (H.toValue lang) $ do
    refliHead' defaultOptions (Just url) "Lex Iterata" ""
    H.body ! A.class_ "u-container-vertical cover" $ do
        prototypeRefliHeader lang (prototypeRefliMainNav' mhTexts)

        div "u-container" $ do
            div "flow-all" $ do
              H.h3 "Lex Iterata"
              H.small ! A.class_ "breadcrumb" $ H.text breadcrumb

            if dense
              then
                div "c-text" $
                  forM_ entries' $ \(i, Entry {..}) ->
                    div "mb-entry" $ do
                      div "mb-num" $
                        H.p . H.text $ show i <> "."
                      div "mb-grow" $ do
                        H.p $ H.text entryTitle
                        div "mb-second-line mb-second-line--dense" $ do
                          H.p $
                            H.small $ do
                              H.text $ "Publié le " <> entryPublicationDate
                              " "
                              H.text entrySource
                          H.p $
                            H.small $ do
                              H.span $ H.a ! A.href (H.toValue $ "/" <> lang <> "/lex/" <> entryId) $
                                "View"
                              H.span $
                                maybe
                                  "Moniteur"
                                  (\lnk -> H.a ! A.href (H.toValue lnk) $ "Moniteur")
                                  entryJournalLink
                              H.span $ H.a ! A.href (H.toValue entryJustelLink) $ "Justel"
              else
                div "c-text limit-42em" $
                  forM_ entries' $ \(i, Entry {..}) ->
                    div "mb-entry" $ do
                      div "mb-num" $
                        H.p . H.text $ show i <> "."
                      H.div $ do
                        H.p $ H.text entryTitle
                        div "mb-second-line" $ do
                          H.p $
                            H.small $ do
                              H.text $ "Publié le " <> entryPublicationDate
                              H.span $
                                maybe
                                  "Moniteur"
                                  (\lnk -> H.a ! A.href (H.toValue lnk) $ "Moniteur")
                                  entryJournalLink
                              H.span $ H.a ! A.href (H.toValue entryJustelLink) $ "Justel"
                          H.p $
                            H.small ""

            H.br
            H.br
            H.p $ do
                H.small ! A.class_ "breadcrumb" $ H.text "Autres années:"
                H.small $
                  mapM_
                    (\year -> do
                      H.text " "
                      H.a ! A.href (H.toValue @Text $ show year) $ H.text (show year)
                    )
                    years

        let MotherboardDocumentFooterTexts {..} = case lang of
              "en" -> motherboardDocumentFooterTextsEn
              "nl" -> motherboardDocumentFooterTextsNl
              _ -> motherboardDocumentFooterTextsFr
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
                  H.a ! A.href (H.toValue $ "https://www.ejustice.just.fgov.be/eli/" <> fragmentFr) $
                    H.text motherboardDocumentFooterOriginalPage
                  H.text $ " " <> motherboardDocumentFooterJournal

        prototypeRefliFooter lang url nbTexts
