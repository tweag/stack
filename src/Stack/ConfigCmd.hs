{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Make changes to the stack yaml file

module Stack.ConfigCmd
       ( ConfigCmdOpts(..)
       , ConfigCmdSetOpts(..)
       , cfgSetField
       , cfgCmdSetName
       , cfgCmdName) where

import Control.Exception (assert)
import Control.Exception.Enclosed (handleIO, catchAny)
import Control.Monad (liftM, when)
import Control.Monad.Catch (MonadMask, throwM, MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Builder         as B
import qualified Data.ByteString.Lazy            as L
import Data.Text
import qualified Data.Text as T
import Network.HTTP.Client.Conduit (HasHttpManager)
import Path
import Path.IO
import Stack.Constants
import Stack.Types
import Stack.Init
import qualified Data.Yaml                   as Yaml
import Debug.Trace

data ConfigCmdOpts = ConfigCmdOpts Text

data ConfigCmdSetOpts = ConfigCmdSetOpts AbstractResolver

cfgSetField :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, HasGHCVariant env, MonadLogger m, MonadBaseControl IO m)
            => (k, v) -> Path Abs Dir -> ConfigCmdSetOpts -> m ()
cfgSetField _ currDir (ConfigCmdSetOpts resolver) = do
    let dest =
            currDir </> stackDotYaml
    exists <- fileExists dest
    let fp = toFilePath $ dest
    (ProjectAndConfigMonoid project _, warnings) <-
        liftIO (Yaml.decodeFileEither fp) >>= either throwM return
    liftIO $ print resolver
    liftIO $ print project
    latestResolver <- makeConcreteResolver resolver
    let project' = project {projectResolver = latestResolver}
    liftIO $ print project'
    liftIO $ L.writeFile fp $ B.toLazyByteString $ renderStackYaml project'
    return ()

cfgCmdName :: String
cfgCmdName = "config"

cfgCmdSetName :: String
cfgCmdSetName = "set"
