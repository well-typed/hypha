module Hypha.BuildEnv.Compose
  ( composeBuildEnv
  ) where

import qualified Data.Set as Set
import Hypha.BuildEnv.Type (BuildEnv (..))

composeBuildEnv :: Monad m => BuildEnv m -> BuildEnv m -> BuildEnv m
composeBuildEnv primary secondary = BuildEnv
  { discoverInstalledPackages =
      Set.union
        <$> discoverInstalledPackages primary
        <*> discoverInstalledPackages secondary
  , locatePackageSource = \pkg -> do
      mp <- locatePackageSource primary pkg
      case mp of
        Just p  -> pure (Just p)
        Nothing -> locatePackageSource secondary pkg
  , locateRepoTarball = \pkg -> do
      mt <- locateRepoTarball primary pkg
      case mt of
        Just t  -> pure (Just t)
        Nothing -> locateRepoTarball secondary pkg
  , locateHaddockHtml = \pkg -> do
      mh <- locateHaddockHtml primary pkg
      case mh of
        Just h  -> pure (Just h)
        Nothing -> locateHaddockHtml secondary pkg
  , ghcVersion = ghcVersion primary
  }
