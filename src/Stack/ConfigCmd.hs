{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Make changes to the stack yaml file

module Stack.ConfigCmd
       (ConfigCmdGet(..)
       ,ConfigCmdSet(..)
       ,ConfigCmdAdd(..)
       ,cfgSetField
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

data ConfigCmdGet = ConfigCmdGetResolver | ConfigCmdGetConfigMonoid Text
data ConfigCmdSet = ConfigCmdSetResolver AbstractResolver | ConfigCmdSetConfigMonoid Text
data ConfigCmdAdd = ConfigCmdAddExtraDep | ConfigCmdAddPackage

cfgSetField :: ( MonadIO m
               , MonadMask m
               , MonadReader env m
               , HasConfig env
               , HasHttpManager env
               , HasGHCVariant env
               , MonadLogger m
               , MonadBaseControl IO m)
               => (k, v) -> Path Abs Dir -> ConfigCmdSet -> m ()
cfgSetField _ currDir (ConfigCmdSetResolver resolver) = do
    let dest =
            currDir </> stackDotYaml
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
cfgSetField _ currDir (ConfigCmdSetConfigMonoid t) = do
    liftIO . putStrLn $ "Trying to write value to " <> (T.unpack t)
    return ()

cfgCmdName :: String
cfgCmdName = "config"

cfgCmdSetName :: String
cfgCmdSetName = "set"

cfgCmdGetName :: String
cfgCmdGetName = "get"

cfgCmdAddName :: String
cfgCmdAddName = "add"
