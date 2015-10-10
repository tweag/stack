{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Make changes to the stack yaml file

module Stack.ConfigCmd
       (ConfigCmdGet(..)
       ,ConfigCmdSet(..)
       ,ConfigCmdAdd(..)
       ,cfgCmdGet
       ,cfgCmdSet
       ,cfgCmdAdd
       ,cfgCmdGetName
       ,cfgCmdSetName
       ,cfgCmdAddName
       ,cfgCmdName) where

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
import Data.Monoid
import Data.Text
import qualified Data.Text as T
import qualified Data.Yaml                   as Yaml
import Network.HTTP.Client.Conduit (HasHttpManager)
import Path
import Path.IO
import Stack.Constants
import Stack.Init
import Stack.Types

import Debug.Trace

data ConfigCmdGet = ConfigCmdGetConfigMonoid Text
data ConfigCmdSet = ConfigCmdSetResolver AbstractResolver | ConfigCmdSetConfigMonoid Text Text
data ConfigCmdAdd = ConfigCmdAddExtraDep | ConfigCmdAddPackage



cfgCmdGet :: ( MonadIO m
             , MonadMask m
             , MonadReader env m
             , HasConfig env
             , HasBuildConfig env
             , HasHttpManager env
             , HasGHCVariant env
             , MonadLogger m
             , MonadBaseControl IO m)
             => ConfigCmdGet -> m ()
cfgCmdGet _ = do undefined

cfgCmdSet :: ( MonadIO m
             , MonadMask m
             , MonadReader env m
             , HasConfig env
             , HasBuildConfig env
             , HasHttpManager env
             , HasGHCVariant env
             , MonadLogger m
             , MonadBaseControl IO m)
             => ConfigCmdSet -> m ()
cfgCmdSet (ConfigCmdSetResolver resolver) = do
    dest <- bcStackYaml <$> asks getBuildConfig
    -- let dest =
    --         stackRoot </> stackDotYaml
    -- exists <- fileExists dest
    let fp = toFilePath $ dest
    (ProjectAndConfigMonoid project _, warnings) <-
        liftIO (Yaml.decodeFileEither fp) >>= either throwM return
    liftIO $ print project
    latestResolver <- makeConcreteResolver resolver
    let project' = project {projectResolver = latestResolver}
    liftIO $ print project'
    liftIO $ L.writeFile fp $ B.toLazyByteString $ renderStackYaml project'
    return ()
cfgCmdSet (ConfigCmdSetConfigMonoid f v) = do
    liftIO . putStrLn $ "Trying to write value " <> (T.unpack v) <> " to field " <> (T.unpack f)
    return ()


cfgCmdAdd :: ( MonadIO m
             , MonadMask m
             , MonadReader env m
             , HasConfig env
             , HasBuildConfig env
             , HasHttpManager env
             , HasGHCVariant env
             , MonadLogger m
             , MonadBaseControl IO m)
             => ConfigCmdAdd -> m ()
cfgCmdAdd _ = do undefined
                 
cfgCmdName :: String
cfgCmdName = "config"

cfgCmdSetName :: String
cfgCmdSetName = "set"

cfgCmdGetName :: String
cfgCmdGetName = "get"

cfgCmdAddName :: String
cfgCmdAddName = "add"
