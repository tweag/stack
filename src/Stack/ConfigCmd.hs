{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

-- | Make changes to the stack yaml file

module Stack.ConfigCmd
       (ConfigCmdSet(..)
       ,cfgCmdSet
       ,cfgCmdSetName
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
import Data.Data
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml
import Network.HTTP.Client.Conduit (HasHttpManager)
import Path
import Path.IO
import Stack.Constants
import Stack.Init
import Stack.Types

data ConfigCmdSet = ConfigCmdSetResolver AbstractResolver

cfgCmdSet :: ( MonadIO m
             , MonadMask m
             , MonadReader env m
             , HasConfig env
             , HasBuildConfig env
             , HasHttpManager env
             , MonadLogger m)
             => ConfigCmdSet -> m ()
cfgCmdSet (ConfigCmdSetResolver newResolver) = do
    stackYaml <- bcStackYaml <$> asks getBuildConfig
    let stackYamlFp =
            toFilePath stackYaml
    -- We don't need to worry about checking for a valid yaml here
    (projectYamlConfig :: Yaml.Object) <-
        liftIO (Yaml.decodeFileEither stackYamlFp) >>=
        either throwM return
    newResolverText <- resolverName <$> makeConcreteResolver newResolver
    let projectYamlConfig' =
            HMap.insert
                "resolver"
                (Yaml.String newResolverText)
                projectYamlConfig
    liftIO
        (L.writeFile
             stackYamlFp
             (B.toLazyByteString
                  (B.byteString
                       (Yaml.encode projectYamlConfig'))))
    return ()

cfgCmdName :: String
cfgCmdName = "config"

cfgCmdSetName :: String
cfgCmdSetName = "set"
