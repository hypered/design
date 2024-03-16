-- | This generates the same HTML and CSS than @emails/example.mjml@.
--
-- TODO Add a test to make sure this stays true.

module Hypered.Html.Struct.Prototypes.Refli.Email
  ( HtmlEmail (..)
  , HtmlBlock (..)
  , renderEmail
  , exampleEmail
  ) where

import Protolude
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Internal as Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

--------------------------------------------------------------------------------
-- | Our emails have a very simple structure.
data HtmlEmail = HtmlEmail [HtmlBlock]

data HtmlBlock =
    Logo
  | Heading Text
  | Paragraph Text

--------------------------------------------------------------------------------
renderEmail :: HtmlEmail -> Html
renderEmail (HtmlEmail blocks) = original $ mapM_ renderBlock blocks

renderBlock :: HtmlBlock -> Html
renderBlock block = case block of
  Logo -> renderLogo
  Heading content -> renderHeading content
  Paragraph content -> renderParagraph content

--------------------------------------------------------------------------------
original :: Html -> Html
original content = do
  -- TODO In the original, the doctype is written in lowercase.
  H.docType
  H.html ! A.lang "fr"
         ! A.dir "ltr"
         ! A.xmlns "http://www.w3.org/1999/xhtml"
         ! H.customAttribute "xmlns:v" "urn:schemas-microsoft-com:vml"
         ! H.customAttribute "xmlns:o" "urn:schemas-microsoft-com:office:office" $ do
    head'
    body' content

head' :: Html
head' = H.head $ do
  H.title $ mempty
  H.preEscapedText "<!--[if !mso]><!-->"
  H.meta ! H.customAttribute "http-equiv" "X-UA-Compatible"
         ! A.content "IE=edge"
  H.preEscapedText "<!--<![endif]-->"
  H.meta ! H.customAttribute "http-equiv" "Content-Type"
         ! A.content "text/html; charset=UTF-8"
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.style ! A.type_ "text/css" $ H.text $ unlines
    [ "#outlook a {"
    , "  padding: 0;"
    , "    }"
    , ""
    , "    body {"
    , "  margin: 0;"
    , "  padding: 0;"
    , "  -webkit-text-size-adjust: 100%;"
    , "  -ms-text-size-adjust: 100%;"
    , "    }"
    , ""
    , "    table,"
    , "    td {"
    , "  border-collapse: collapse;"
    , "  mso-table-lspace: 0pt;"
    , "  mso-table-rspace: 0pt;"
    , "    }"
    , ""
    , "    img {"
    , "  border: 0;"
    , "  height: auto;"
    , "  line-height: 100%;"
    , "  outline: none;"
    , "  text-decoration: none;"
    , "  -ms-interpolation-mode: bicubic;"
    , "    }"
    , ""
    , "    p {"
    , "  display: block;"
    , "  margin: 13px 0;"
    , "    }"
    ]
  H.preEscapedText "<!--[if mso]>"
  do
    H.noscript $
      Blaze.customParent "xml" $ do
        H.preEscapedText "<o:OfficeDocumentSettings>"
        H.preEscapedText "  <o:AllowPNG/>"
        H.preEscapedText "  <o:PixelsPerInch>96</o:PixelsPerInch>"
        H.preEscapedText "</o:OfficeDocumentSettings>"
  H.preEscapedText "<![endif]-->"
  H.preEscapedText "<!--[if lte mso 11]>"
  do
    H.style ! A.type_ "text/css" $
      H.preEscapedText ".mj-outlook-group-fix { width:100% !important; }"
  H.preEscapedText "<![endif]-->"
  H.preEscapedText "<!--[if !mso]><!-->"
  H.link
    ! A.href "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@0,400;0,600"
    ! A.rel "stylesheet"
    ! A.type_ "text/css"
  H.style ! A.type_ "text/css" $
    H.preEscapedText
      "@import \
      \url(https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@0,400;0,600);"

  H.preEscapedText "<!--<![endif]-->"
  H.style ! A.type_ "text/css" $ H.text $ unlines
    [ "@media only screen and (min-width:480px) {"
    , "  .mj-column-per-100 {"
    , "    width: 100% !important;"
    , "    max-width: 100%;"
    , "  }"
    , "    }"
    ]

  H.style ! A.media "screen and (min-width:480px)" $ H.text $ unlines
    [ ".moz-text-html .mj-column-per-100 {"
    , "  width: 100% !important;"
    , "  max-width: 100%;"
    , "    }"
    ]

  H.style ! A.type_ "text/css" $ H.text $ unlines
    [ "@media only screen and (max-width:479px) {"
    , "  table.mj-full-width-mobile {"
    , "    width: 100% !important;"
    , "  }"
    , ""
    , "  td.mj-full-width-mobile {"
    , "    width: auto !important;"
    , "  }"
    , "    }"
    ]

body' :: Html -> Html
body' content = H.body ! A.style "word-spacing:normal;" $ do
  H.div ! A.style "" ! A.lang "fr" ! A.dir "auto" $ do
    H.preEscapedText "<!--[if mso | IE]><table align=\"center\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"\" role=\"presentation\" style=\"width:600px;\" width=\"600\" ><tr><td style=\"line-height:0px;font-size:0px;mso-line-height-rule:exactly;\"><![endif]-->"
    H.div ! A.style "margin:0px auto;max-width:600px;" $
      H.table ! align "center"
              ! border "0"
              ! cellpadding "0"
              ! cellspacing "0"
              ! A.role "presentation"
              ! A.style "width:100%;" $
        H.tbody $
          H.tr $
            H.td ! A.style "direction:ltr;font-size:0px;padding:20px 0;text-align:center;" $ do
              H.preEscapedText "<!--[if mso | IE]><table role=\"presentation\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\"><tr><td class=\"\" style=\"vertical-align:top;width:600px;\" ><![endif]-->"
              H.div ! A.class_ "mj-column-per-100 mj-outlook-group-fix"
                    ! A.style "font-size:0px;text-align:left;direction:ltr;display:inline-block;vertical-align:top;width:100%;" $
                H.table ! border "0"
                        ! cellpadding "0"
                        ! cellspacing "0"
                        ! A.role "presentation"
                        ! A.style "vertical-align:top;"
                        ! A.width "100%" $
                  H.tbody $
                    content
              H.preEscapedText "<!--[if mso | IE]></td></tr></table><![endif]-->"
    H.preEscapedText "<!--[if mso | IE]></td></tr></table><![endif]-->"

align :: H.AttributeValue -> H.Attribute
align = H.customAttribute "align"

border :: H.AttributeValue -> H.Attribute
border = H.customAttribute "border"

cellpadding :: H.AttributeValue -> H.Attribute
cellpadding = H.customAttribute "cellpadding"

cellspacing :: H.AttributeValue -> H.Attribute
cellspacing = H.customAttribute "cellspacing"

renderLogo :: Html
renderLogo = do
  H.tr $
    H.td ! align "left"
         ! A.style "font-size:0px;padding:10px 25px;word-break:break-word;" $
      H.table ! border "0"
              ! cellpadding "0"
              ! cellspacing "0"
              ! A.role "presentation"
              ! A.style "border-collapse:collapse;border-spacing:0px;" $
        H.tbody $
          H.tr $
            H.td ! A.style "width:60px;" $
              -- TODO In the original, the img is self-closing.
              H.img ! A.alt ""
                    ! A.src "https://refli.be/static/images/logo-120px.png"
                    ! A.style "border:0;display:block;outline:none;text-decoration:none;height:auto;width:100%;font-size:13px;"
                    ! A.width "60"
                    ! A.height "auto"
  H.tr $
    H.td ! A.style "font-size:0px;word-break:break-word;" $
      H.div ! A.style "height:30px;line-height:30px;" $
        H.preEscapedText "&#8202;"

renderHeading :: Text -> Html
renderHeading content =
  H.tr $
    H.td ! align "left"
         ! A.style "font-size:0px;padding:10px 25px;word-break:break-word;" $
      H.div ! A.style "font-family:IBM Plex Sans, Arial;font-size:25.6288px;font-weight:600;line-height:1;text-align:left;color:#000000;" $
        H.text content

renderParagraph :: Text -> Html
renderParagraph content =
  H.tr $
    H.td ! align "left"
         ! A.style "font-size:0px;padding:10px 25px;word-break:break-word;" $
      H.div ! A.style "font-family:IBM Plex Sans, Arial;font-size:16px;line-height:1.5;text-align:left;color:#000000;" $
        H.text content

--------------------------------------------------------------------------------

-- | This should correspond to @emails/example.mjml@.
exampleEmail :: HtmlEmail
exampleEmail = HtmlEmail
    [ Logo
    , Heading "Nisl vitae vitae in at"
    , Paragraph "Nunc consequat sodales purus aenean mattis at et risus nec. Lorem pretium sagittis odio faucibus imperdiet. Nisi, commodo proin accumsan, quis placerat. Vitae sed. Porta leo phasellus tristique augue odio et fringilla. Feugiat cursus id euismod eu aliquet quam. Nam tempor iaculis urna amet et commodo, arcu porta lectus. Sodales a netus dolor sociis nunc"
    , Paragraph "Odio eget condimentum cras non. Nisl vitae vitae in at ultrices nibh imperdiet risus. Ac tristique nibh praesent id auctor neque, ut sit. Dignissim facilisis ut pharetra, vel nibh habitant suspendisse. Vel id viverra ornare sagittis morbi pretium laoreet sit. Dignissim odio condimentum arcu risus scelerisque scelerisque in."
    ]
