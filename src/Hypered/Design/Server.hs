{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Hypered.Design.Server
  ( run
  ) where

import qualified Commence.Runtime.Errors       as Errs
import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import           Data.Aeson
import qualified Hypered.Design.Command        as Command
import qualified Hypered.Design.Fluid          as Fluid
import qualified Hypered.Html                  as Hy
import qualified Network.HTTP.Types.Status     as Status
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import           Protolude               hiding ( Handler )
import           Servant                 hiding ( serve )
import           Servant.HTML.Blaze             ( HTML )
import qualified Servant.Server                as Server
import           System.FilePath                ( (</>) )
import qualified System.Systemd.Daemon         as SD
import           Text.Blaze.Html5               ( Html, (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           WaiAppStatic.Storage.Filesystem
                                                ( defaultWebAppSettings )
import           WaiAppStatic.Storage.Filesystem.Extended
                                                ( hashFileIfExists
                                                , webAppLookup
                                                )
import           WaiAppStatic.Types             ( ss404Handler
                                                , ssLookupFile
                                                , ssMaxAge
                                                , MaxAge(NoMaxAge)
                                                )
import           Web.FormUrlEncoded             ( FromForm(..)
                                                , parseUnique
                                                )


--------------------------------------------------------------------------------
type Runtime = ()

newtype AppM a = AppM { runAppM :: ReaderT Runtime (ExceptT Errs.RuntimeErr IO) a }
  deriving newtype
           ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Runtime
           , MonadError Errs.RuntimeErr
           , MonadThrow
           , MonadCatch
           , MonadMask
           )



--------------------------------------------------------------------------------
-- | This is the main Servant API definition for the design server.
-- Only the /echo routes will be exposed through Nginx at first.
type App =    "" :> Raw
         :<|> "echo" :> Get '[HTML] Html
         :<|> "echo" :> "login"
              :> ReqBody '[FormUrlEncoded] Login
              :> Post '[HTML] EchoPage

         :<|> "fluid" :> "type" :> Get '[HTML] Html

              -- Call here the page you want to work on.
         :<|> "edit" :> Get '[HTML] Html

         :<|> Raw -- Fallback handler for the static files, in particular the
                  -- documentation.


--------------------------------------------------------------------------------
-- | This is the main Servant server definition, corresponding to @App@.
serverT
  :: forall m
   . ServerC m
  => FilePath -- ^ Path to the static directory (for the documentation)
  -> ServerT App m
serverT root =
  showHomePage root
    :<|> showEchoIndex
    :<|> echoLogin
    :<|> showTypeScaleGenerate
    :<|> edit -- Call here the page you want to work on.
    :<|> serveDocumentation root


--------------------------------------------------------------------------------
-- | Minimal set of constraints needed on some monad @m@ to be satisfied to be
-- able to run a server.
type ServerC m
  = ( MonadIO m
    )


--------------------------------------------------------------------------------
type ServerSettings = '[]

-- | This is the main function of this module. It runs a Warp server, serving
-- our @App@ API definition.
run
  :: forall m
   . MonadIO m
  => Command.ServerConf
  -> m ()
run Command.ServerConf {..} = liftIO $ do
  let warpSettings =
        Warp.setPort _serverPort $
        Warp.setBeforeMainLoop (SD.notifyReady >> pure ())
        Warp.defaultSettings
  Warp.runSettings warpSettings waiApp
 where
  waiApp = serve @AppM appMHandlerNatTrans ctx _serverStaticDir
  ctx = Server.EmptyContext

-- | Turn our @serverT@ definition into a Wai application, suitable for
-- Warp.run.
serve
  :: forall m
   . ServerC m
  => (forall x . m x -> Handler x) -- ^ Natural transformation to transform an
                                   -- arbitrary @m@ to a Servant @Handler@.
  -> Server.Context ServerSettings
  -> FilePath -- ^ Path to the static directory (for the documentation)
  -> Wai.Application
serve handlerNatTrans ctx root =
  Servant.serveWithContext appProxy ctx
    $ hoistServerWithContext appProxy settingsProxy handlerNatTrans
    $ serverT root

appProxy :: Proxy App
appProxy = Proxy @App

settingsProxy :: Proxy ServerSettings
settingsProxy = Proxy @ServerSettings

-- | Natural transformation from some `AppM`, to a Servant handler.
appMHandlerNatTrans :: forall a . AppM a -> Servant.Handler a
appMHandlerNatTrans appM =
  let 
      -- We peel off the AppM + ReaderT layers, exposing our ExceptT
      -- RuntimeErr IO a This is very similar to Servant's Handler:
      -- https://hackage.haskell.org/package/servant-server-0.17/docs/Servant-Server-Internal-Handler.html#t:Handler
      unwrapReaderT          = (`runReaderT` ()) . runAppM $ appM
      -- Map our errors to `ServantError`
      runtimeErrToServantErr = withExceptT Errs.asServantError
  in  
      -- Re-wrap as servant `Handler`
      Servant.Handler $ runtimeErrToServantErr unwrapReaderT


--------------------------------------------------------------------------------
showHomePage :: forall m . ServerC m => FilePath -> Tagged m Application
showHomePage root = Tagged $ \_ res -> res $
  Wai.responseFile
    Status.status200
    [("Content-Type", "text/html")]
    (root </> "index.html")
    Nothing


--------------------------------------------------------------------------------
showEchoIndex :: forall m . ServerC m => m Html
showEchoIndex = pure "Echo."

--------------------------------------------------------------------------------
-- | Serve the static files for the documentation. This also provides a custom
-- 404 fallback.
serveDocumentation :: ServerC m => FilePath -> Tagged m Application
serveDocumentation root = serveDirectoryWith settings
 where
  settings = (defaultWebAppSettings root)
    { ss404Handler = Nothing -- Just custom404
    , ssLookupFile = webAppLookup hashFileIfExists root
    , ssMaxAge = NoMaxAge
    }

{-
custom404 :: Application
custom404 _request sendResponse = sendResponse $ Wai.responseLBS
  HTTP.status404
  [("Content-Type", "text/html; charset=UTF-8")]
  (renderMarkup $ H.toMarkup Pages.NotFoundPage)
-}


--------------------------------------------------------------------------------
-- | Represents the input data to log in a user.
data Login = Login
  { _loginUsername :: Text
  , _loginPassword :: Text
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm Login where
  fromForm f = Login <$> parseUnique "username" f <*> parseUnique "password" f

data EchoPage = EchoPage
  { _echoPageContent     :: Text
    -- ^ Text, displayed as code.
  }

instance H.ToMarkup EchoPage where
  toMarkup EchoPage {..} =
    H.pre . H.code $ H.text _echoPageContent

echoLogin :: ServerC m => Login -> m EchoPage
echoLogin = pure . EchoPage . show

edit :: ServerC m => m Html
edit = pure $ Hy.document "Refli" Hy.homePageRefli

--------------------------------------------------------------------------------
showTypeScaleGenerate :: ServerC m => m Html
showTypeScaleGenerate = pure $ do
  H.docType
  H.html ! A.dir "ltr" ! A.lang "en" $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.link ! A.rel "stylesheet" ! A.href "/static/css/struct/foundations.css"
      H.link ! A.rel "stylesheet" ! A.href "/static/css/struct/ibm-plex.css"
      -- Normally we link the stylesheet "/static/css/struct/scale.css"
      -- but the point of this page is to generate (alternatives of) it.
      H.style . H.text $ Fluid.everything
      H.link ! A.rel "stylesheet" ! A.href "/static/css/struct/misc.css"
    H.body $ do
        classes "u-container" $ do
          classes "c-text flow-all" $ do
            H.h1 "Type scale"
            H.p $ do
              "This page shows the type scale, from "
              H.code "h1"
              " to "
              H.code "p"
              ", for the parameters XXX."

          classes "c-text flow-all" $ do
            H.h1 "I am a heading"
            H.h2 "I am a heading"
            H.h3 "I am a heading"
            H.h4 "I am a heading"
            H.p "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
            H.p $
              H.small "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

classes :: Text -> Html -> Html
classes xs = H.div ! A.class_ xs'
 where
  xs' = H.toValue xs
