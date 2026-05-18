{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Hoogle.Database
  ( HoogleConfig (..)
  , dbPath
  , planHashFile
  , withProjectDb
  , withGlobalDb
  , isStale
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import qualified Hoogle

import Hypha.Types.BuildPlan (ProjectRoot (..))

-- | Configuration for the per-project Hoogle database.
data HoogleConfig = HoogleConfig
  { hgcProjectRoot :: !ProjectRoot
  , hgcInputDocs   :: ![FilePath]
    -- ^ Paths to per-package .txt Hoogle inputs.
  , hgcPlanHash    :: !Text
    -- ^ Digest of plan.json contents.
  }
  deriving stock (Show, Eq)

-- | Path to the per-project Hoogle database file.
dbPath :: ProjectRoot -> FilePath
dbPath (ProjectRoot r) = r </> ".hypha" </> "hoogle.hoo"

-- | Path to the file storing the plan hash for staleness detection.
planHashFile :: ProjectRoot -> FilePath
planHashFile (ProjectRoot r) = r </> ".hypha" </> "plan-hash"

-- | Open the per-project DB, generating it if missing or stale.
withProjectDb :: NFData a => HoogleConfig -> (Hoogle.Database -> IO a) -> IO a
withProjectDb cfg k = do
  let root = hgcProjectRoot cfg
      dotDir = (\(ProjectRoot r) -> r </> ".hypha") root
  createDirectoryIfMissing True dotDir
  stale <- isStale cfg
  if stale
    then do
      let input = case hgcInputDocs cfg of
            (p:_) -> p
            []    -> "."
      Hoogle.hoogle
        [ "generate"
        , "--database=" <> dbPath root
        , "--local=" <> input
        ]
      BS.writeFile (planHashFile root) (TE.encodeUtf8 (hgcPlanHash cfg))
    else pure ()
  Hoogle.withDatabase (dbPath root) k

-- | Open the global (Stackage-built) Hoogle database.
withGlobalDb :: NFData a => (Hoogle.Database -> IO a) -> IO a
withGlobalDb k = do
  path <- Hoogle.defaultDatabaseLocation
  Hoogle.withDatabase path k

-- | Check if the per-project DB is stale relative to the plan hash.
isStale :: HoogleConfig -> IO Bool
isStale cfg = do
  let pf = planHashFile (hgcProjectRoot cfg)
      df = dbPath (hgcProjectRoot cfg)
  pfOk <- doesFileExist pf
  dfOk <- doesFileExist df
  if not (pfOk && dfOk)
    then pure True
    else do
      saved <- BS.readFile pf
      pure (saved /= TE.encodeUtf8 (hgcPlanHash cfg))
