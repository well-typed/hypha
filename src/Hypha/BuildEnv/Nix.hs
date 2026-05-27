module Hypha.BuildEnv.Nix
  ( mkNixBuildEnv
  ) where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..), renderPackageId)

-- | Build a 'BuildEnv' backed by a Nix store path (or a @result@
-- symlink pointing to one).  We discover installed packages by
-- reading GHC @package.conf.d@ entries directly.
mkNixBuildEnv :: FilePath -> BuildEnv IO
mkNixBuildEnv !resultPath = BuildEnv
  { discoverInstalledPackages = discoverNixPackages resultPath
  , locatePackageSource       = \_ -> pure Nothing
  , locateRepoTarball         = \_ -> pure Nothing
  , locateHaddockHtml         = findNixHaddock resultPath
  , ghcVersion                = discoverNixGhcVersion resultPath
  }

-------------------------------------------------------------------------------
-- Package discovery

discoverNixPackages :: FilePath -> IO (Set PackageId)
discoverNixPackages resultPath = do
  mPkgConfDir <- findGhcPackageConfDir resultPath
  case mPkgConfDir of
    Nothing   -> pure Set.empty
    Just dir  -> do
      entries <- listDirectory dir
      let confs = filter (\e -> takeExtension e == ".conf") entries
      fmap Set.fromList . mapMaybeM (parsePkgIdFromConf . (dir </>)) $ confs

findGhcPackageConfDir :: FilePath -> IO (Maybe FilePath)
findGhcPackageConfDir root = do
  let libDir = root </> "lib"
  exists <- doesDirectoryExist libDir
  if not exists then pure Nothing else do
    entries <- listDirectory libDir
    let ghcDirs = filter (\e -> Text.isPrefixOf (Text.pack "ghc-") (Text.pack e)) entries
    case ghcDirs of
      []      -> pure Nothing
      (d : _) -> do
        let pkgConfDir = libDir </> d </> "package.conf.d"
        ok <- doesDirectoryExist pkgConfDir
        pure $ if ok then Just pkgConfDir else Nothing

parsePkgIdFromConf :: FilePath -> IO (Maybe PackageId)
parsePkgIdFromConf !path = do
  content <- TIO.readFile path
  let ls   = map Text.strip (Text.lines content)
      mName = findValue (Text.pack "name") ls
      mVer  = findValue (Text.pack "version") ls
  pure $ PackageId <$> (PackageName <$> mName) <*> (Version <$> mVer)

findValue :: Text -> [Text] -> Maybe Text
findValue !key = listToMaybe . mapMaybe go
  where
    go :: Text -> Maybe Text
    go line =
      case Text.breakOn (Text.pack ":") line of
        (_, rest) | Text.null rest -> Nothing
        (k, rest) ->
          if Text.strip k == key
            then Just (Text.strip (Text.drop 1 rest))
            else Nothing

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = do
  mbs <- mapM f xs
  pure [ b | Just b <- mbs ]

-------------------------------------------------------------------------------
-- Haddock discovery

findNixHaddock :: FilePath -> PackageId -> IO (Maybe FilePath)
findNixHaddock !root !pkg = do
  let candidates =
        [ root </> "share" </> "doc" </> Text.unpack (renderPackageId pkg) </> "html" </> "index.html"
        , root </> "share" </> "doc" </> "html" </> Text.unpack (renderPackageId pkg) </> "index.html"
        ]
  findM doesFileExist candidates

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _   []     = pure Nothing
findM p (x : xs) = do
  ok <- p x
  if ok then pure (Just x) else findM p xs

-------------------------------------------------------------------------------
-- GHC version discovery

discoverNixGhcVersion :: FilePath -> IO Version
discoverNixGhcVersion !resultPath = do
  let libDir = resultPath </> "lib"
  exists <- doesDirectoryExist libDir
  if not exists
    then pure (Version (Text.pack "unknown"))
    else do
      entries <- listDirectory libDir
      case filter (\e -> Text.isPrefixOf (Text.pack "ghc-") (Text.pack e)) entries of
        []   -> pure (Version (Text.pack "unknown"))
        (d:_) -> case Text.stripPrefix (Text.pack "ghc-") (Text.pack d) of
          Nothing -> pure (Version (Text.pack "unknown"))
          Just v  -> pure (Version v)
