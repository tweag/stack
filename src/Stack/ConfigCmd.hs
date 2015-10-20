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
import Data.Data
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml
import Data.Yaml ((.=))
import GHC.Generics
import Data.Generics.Uniplate.Data
import Language.Haskell.TH
import Network.HTTP.Client.Conduit (HasHttpManager)
import Path
import Path.IO
import Stack.Constants
import Stack.Init
import Stack.Types
import Data.Function ((&))

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
cfgCmdGet (ConfigCmdGetConfigMonoid field) = do
    stackYaml <- bcStackYaml <$> asks getBuildConfig
    let stackYamlFp =
            toFilePath stackYaml
    (ProjectAndConfigMonoid project cfg,warnings) <-
        liftIO (Yaml.decodeFileEither stackYamlFp) >>=
        either throwM return
    let smap =
            Map.fromList
                (zip
                     (fmap cfgName configMonoidYamlFieldNames)
                     (fmap (cfgValueText) configMonoidYamlFieldNames))
    let val =
            case Map.lookup field smap of
                -- "resolver" ->
                --     T.unpack . resolverName $ projectResolver project
                Just sel ->
                    B.byteString . TE.encodeUtf8 $ sel cfg  -- show $ (cfgValue configMonoidDockerOptsField) cfg
                Nothing ->
                    B.byteString "not here"
    let projMap =
            case Yaml.toJSON project of
                (Yaml.Object o) ->
                    HMap.lookup field o &
                    \case
                        Just (o) ->
                            show $ Yaml.encode o
                        Nothing ->
                            "not here"
    liftIO . putStrLn $ projMap
    liftIO . L.putStr . B.toLazyByteString $ val
    liftIO . L.putStr . B.toLazyByteString . B.byteString  $ Yaml.encode cfg
    liftIO . print . universe $ Yaml.toJSON cfg
    liftIO . print . universe $ Yaml.toJSON project
    return ()

configMonoidYamlFieldNames :: [ConfigMonoidField]
configMonoidYamlFieldNames =
    $(do nameDs <-
             mapM lookupValueName cfgSelectorsYamlDeclNames
         fieldNames <-
             mapM
                 (\case
                      (Just cfgField,_) ->
                          runQ [|$(varE cfgField)|]
                      (Nothing,nameD) ->
                          fail $
                          "Compile time config error: no top level declaration " ++
                          nameD ++
                          ".\n    Please define one in Stack.Config.Types")
                 (zip nameDs cfgSelectorsYamlDeclNames)
         pure $
             ListE fieldNames)

foo :: [ConfigMonoid -> String]
foo =
    $(do nameDs <-
             mapM (lookupValueName . fst) cfgSelectors
         x <-
             mapM
                 (\case
                      (Just cfgName,_) ->
                          runQ [|show . $(varE cfgName)|]
                      (Nothing,nameD) ->
                          fail $
                          "Compile time config error: no top level declaration " ++
                          nameD ++
                          ".\n    Please define one in Stack.Config.Types")
                 (zip nameDs (fmap fst cfgSelectors))
         pure $
             ListE x)

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
    liftIO $ print projectYamlConfig
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
cfgCmdSet (ConfigCmdSetConfigMonoid f v) = do
    liftIO . putStrLn $ "Trying to write value " <> T.unpack v <> " to field " <>
        T.unpack f
    return ()

encodeValueAsBuilder :: Yaml.Value -> B.Builder
encodeValueAsBuilder = B.byteString . Yaml.encode

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

