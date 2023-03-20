-- | This module defines a set of helper functions to write HTML to disk. The
-- result is used as a static site at hypered.design.
module Hypered.Design.IO where

import           Hypered.Html
  ( Config(..)
  , defaultConfig
  , documentFile
  )

import qualified Data.Text.Lazy.IO as T
import           Protolude
import System.FilePath (takeDirectory, (</>))
import System.Directory (createDirectoryIfMissing)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)


------------------------------------------------------------------------------
-- | Generate both the normal and pretty-printed HTML versions.
generate :: FilePath -> Text -> (FilePath -> Html) -> IO ()
generate path title body = do
  generate' path title defaultConfig body

-- | Use this version of `generate` and pass it False to render complete
-- exemple pages (i.e. without including the wrapper to present the design
-- system components).
generate' :: FilePath -> Text -> Config -> (FilePath -> Html) -> IO ()
generate' path title conf body = do
  generateHtml conf "generated/min" path title (body path)
  prettyHtml conf "generated/pretty" path title (body path)
  partialHtml conf "generated/partial" path title (body path)


------------------------------------------------------------------------------
generateHtml :: Config -> FilePath -> FilePath -> Text -> Html -> IO ()
generateHtml config base path title body = do
  createDirectoryIfMissing True (takeDirectory (base </> path))
  withFile (base </> path) WriteMode $ \h ->
    T.hPutStr h . renderHtml $ documentFile config path title body

-- The result of this function is used to populate the static pages at
-- hypered.design/hs.
prettyHtml :: Config -> FilePath -> FilePath -> Text -> Html -> IO ()
prettyHtml config base path title body = do
  createDirectoryIfMissing True (takeDirectory (base </> path))
  withFile (base </> path) WriteMode $ \h ->
    hPutStr h . Pretty.renderHtml $ documentFile config path title body'
  where body' = do
          if cAddWrapper config
            then H.div ! A.class_ "pa3 pa4-ns" $ body
            else body

-- | Same as prettyHtml but doesn't wrap the content to create a full
-- standalone HTML document.
partialHtml :: Config -> FilePath -> FilePath -> Text -> Html -> IO ()
partialHtml _ base path _ body = do
  createDirectoryIfMissing True (takeDirectory (base </> path))
  withFile (base </> path) WriteMode $ \h ->
    hPutStr h . Pretty.renderHtml $ body
