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
import qualified Network.HTTP.Types.Status     as Status
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import           Protolude               hiding ( Handler )
import qualified Hypered.Design.Command        as Command
import           Servant                 hiding ( serve )
import qualified Servant.Server                as Server
import           System.FilePath                ( (</>) )
import qualified System.Systemd.Daemon         as SD
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


--------------------------------------------------------------------------------
-- | This is the main Servant API definition for Refli.
type App =    "" :> Raw
         :<|> Raw -- Fallback handler for the static files, in particular the
                  -- documentation.


--------------------------------------------------------------------------------
type Runtime = ()

newtype AppM a = AppM { runAppM :: ReaderT Runtime (ExceptT Errs.RuntimeErr IO) a }
  deriving ( Functor
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
-- | This is the main Servant server definition, corresponding to @App@.
serverT
  :: forall m
   . ServerC m
  => FilePath -- ^ Path to the static directory (for the documentation)
  -> ServerT App m
serverT root =
  showHomePage root
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