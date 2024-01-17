module Hypered.Html.Struct.Prototypes.Refli.LandingPage where

import qualified Data.Text as T
import Hypered.Html.Common (
  autoReload,
 )
import Hypered.Html.Helpers
import Hypered.Html.Struct.Prototypes.Refli.Common as Struct
import Protolude hiding (div)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.HTML.TagSoup (innerText, Tag(..), (~==))

--------------------------------------------------------------------------------
data LandingPageTexts = LandingPageTexts
  { landingPageLanguage :: Text
  , landingPageTitle :: Text
  , landingPageDescription :: Text
  }

prototypeRefliLandingPage :: Bool -> LandingPageTexts -> Html
prototypeRefliLandingPage autoreload LandingPageTexts {..} = do
  refliDocument autoreload landingPageLanguage landingPageTitle landingPageDescription $
    H.body ! A.class_ "u-container-vertical cover" $ do
      H.text landingPageTitle
