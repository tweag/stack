{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Make changes to the stack yaml file

module Stack.ConfigCmd
       ( ConfigCmdSetOpts(..)
       , ConfigCmdSetField(..)
       , ConfigCmdSetValue(..)
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

data ConfigCmdSetOpts = ConfigCmdSetOpts ConfigCmdSetField ConfigCmdSetValue

data ConfigCmdSetField = ConfigCmdSetResolver AbstractResolver | ConfigCmdSetConfigMonoid Text

data ConfigCmdSetValue = ConfigCmdSetValue Text


cfgSetField :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, HasGHCVariant env, MonadLogger m, MonadBaseControl IO m)
            => (k, v) -> Path Abs Dir -> ConfigCmdSetOpts -> m ()
cfgSetField _ currDir (ConfigCmdSetOpts field value) = do
    let dest =
            currDir </> stackDotYaml
        resolver = undefined
    exists <- fileExists dest
    let fp = toFilePath $ dest
    (ProjectAndConfigMonoid project _, warnings) <-
        liftIO (Yaml.decodeFileEither fp) >>= either throwM return
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
