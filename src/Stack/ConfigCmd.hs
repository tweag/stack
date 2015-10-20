{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Make changes to the stack yaml file

module Stack.ConfigCmd
       (ConfigCmdSet(..)
       ,cfgCmdSet
       ,cfgCmdSetName
       ,cfgCmdName) where

import           Control.Monad.Catch (MonadMask, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, asks)
import qualified Data.ByteString.Builder         as B
import qualified Data.ByteString.Lazy            as L
import qualified Data.HashMap.Strict as HMap
import qualified Data.Yaml as Yaml
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Stack.Init
import           Stack.Types

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
    -- We do need to check to ensure the build plan is valid  
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
