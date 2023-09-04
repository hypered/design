-- | Alternative to Hypered.Html.Tachyons, using the struct.css CSS instead.
module Hypered.Html.Struct where

import Protolude hiding (div)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- Specimen: navigation.
-- TODO Move to Hypered.Design.Struct.Specimens.
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

-- Prototype: motherboard-index-1.
-- TODO Move to Hypered.Design.Struct.Prototypes
prototypeMotherboardIndex1 :: Html
prototypeMotherboardIndex1 = prototypeMotherboardIndex False "Constitution / 1831" [entry]
 where
  entry = Entry {..}
  entryTitle = "7 FEVRIER 1831. - CONSTITUTION DE LA BELGIQUE."
  entrySource = ""
  entryPublicationDate = "07-02-1831"
  entryJournalLink = Nothing
  entryJustelLink =
    "https://www.ejustice.just.fgov.be/eli/constitution/1831/02/07/1831020701/justel"

-- Prototype: motherboard-index-dense.
prototypeMotherboardIndexDense :: Html
prototypeMotherboardIndexDense = prototypeMotherboardIndex True "Loi / 2022"
  [entry1, entry2, entry3, entry4]
 where
  entry1 = Entry
    { entryTitle = "19 JANVIER 2022. - Loi portant le livre 2, titre 3, \"Les relations patrimoniales des couples\" et le livre 4 \"Les successions, donations et testaments\" du Code civil (1)"
    , entryPublicationDate = "14-03-2022"
    , entrySource = "SERVICE PUBLIC FEDERAL JUSTICE"
    , entryJournalLink = Just
        "https://www.ejustice.just.fgov.be/eli/loi/2022/01/19/2022030600/moniteur"
    , entryJustelLink =
        "https://www.ejustice.just.fgov.be/eli/loi/2022/01/19/2022030600/justel"
    }
  entry2 = Entry
    { entryTitle = "19 JANVIER 2022. - CODE CIVIL - LIVRE 2, Titre 3 : \" Les relations patrimoniales des couples \""
    , entryPublicationDate = "14-03-2022"
    , entrySource = "JUSTICE"
    , entryJournalLink = Nothing
    , entryJustelLink =
        "https://www.ejustice.just.fgov.be/eli/loi/2022/01/19/2022A30600/justel"
    }
  entry3 = Entry
    { entryTitle = "19 JANVIER 2022. - CODE CIVIL - LIVRE 4 : \" Les successions, donations et testaments \""
    , entryPublicationDate = "14-03-2022"
    , entrySource = "JUSTICE"
    , entryJournalLink = Nothing
    , entryJustelLink =
        "https://www.ejustice.just.fgov.be/eli/loi/2022/01/19/2022B30600/justel"
    }
  entry4 = Entry
    { entryTitle = "21 JANVIER 2022. - Loi portant des dispositions fiscales diverses (1)"
    , entryPublicationDate = "28-01-2022"
    , entrySource = "SERVICE PUBLIC FEDERAL FINANCES"
    , entryJournalLink = Just
        "http://www.ejustice.just.fgov.be/eli/loi/2022/01/21/2022040046/moniteur"
    , entryJustelLink =
        "http://www.ejustice.just.fgov.be/eli/loi/2022/01/21/2022040046/justel"
    }

data Entry = Entry
  { entryTitle :: Text
  , entryPublicationDate :: Text
  , entrySource :: Text
  , entryJournalLink :: Maybe Text
  , entryJustelLink :: Text
  }

prototypeMotherboardIndex :: Bool -> Text -> [Entry] -> Html
prototypeMotherboardIndex dense breadcrumb entries = do
  let entries' = zip [(1 :: Int) ..] entries
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
                            H.a ! A.href "/lex" $ "Lex Iterata"

        div "u-container" $ do
            H.p $
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

        H.footer $
            div "u-container" $ do
                H.hr
                div "c-text flow" $ do
                    H.p $
                        H.small $ do
                          "Lex Iterata is a Refli experiment. "
                          H.a ! A.href "/lex" $ "Read more."
                    H.p "© Hypered SRL, 2023."

div :: H.AttributeValue -> Html -> Html
div cls = H.div ! A.class_ cls

-- include ../includes/main-header--page-struct
mainHeaderPageStruct :: Html
mainHeaderPageStruct = H.p "TODO"
