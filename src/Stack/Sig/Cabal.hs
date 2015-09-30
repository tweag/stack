{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

{-|
Module      : Stack.Sig.Cabal
Description : Cabal Functions
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Cabal (cabalFilePackageId) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>), (<*>))
#endif

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.Version as V
import qualified Distribution.Package as D
import qualified Distribution.PackageDescription as D
import qualified Distribution.PackageDescription.Parse as D
import qualified Distribution.Verbosity as D
import           Stack.Types

-- | Extract the @PackageIdentifier@ given an exploded haskell package
-- path.
cabalFilePackageId
    :: (MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadMask m, MonadLogger m, MonadThrow m)
    => FilePath -> m PackageIdentifier
cabalFilePackageId fp = do
    pkgDescr <- liftIO (D.readPackageDescription D.silent fp)
    (toStackPI . D.package . D.packageDescription) pkgDescr
  where
    toStackPI (D.PackageIdentifier (D.PackageName name) ver) =
        PackageIdentifier <$> parsePackageNameFromString name <*>
        parseVersionFromString (V.showVersion ver)
