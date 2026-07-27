{-# LANGUAGE DerivingStrategies #-}
-- | Typed views for the server's module and symbol pages.
--
-- 'ModuleDocView' encodes the documentation-priority chain as a sum:
-- prebuilt Haddock when we found rendered docs on disk, on-the-fly
-- source extraction otherwise, and a bare export list only as the
-- last resort — with the reason for the degradation carried along,
-- never swallowed.
module Hypha.Server.ModuleDoc
  ( ModuleDocView (..)
  , PrebuiltDoc (..)
  , SourceDoc (..)
  , SymbolCardData (..)
  ) where

import Data.Text (Text)

import qualified Hypha.Source.Extract as Extract
import           Hypha.Source.Locate  (Provenance)
import           Hypha.Source.Parser  (DeclKind)

-- | What the module page renders, in priority order.
data ModuleDocView
  = ViewPrebuilt !PrebuiltDoc
    -- ^ Rendered Haddock exists on disk; embed its content regions.
  | ViewFromSource !SourceDoc
    -- ^ No prebuilt docs — render the documentation straight from the
    -- module source (no extra disk space needed).
  | ViewExportsOnly ![Text] !Text
    -- ^ Last resort: export names plus the human-readable reason we
    -- could not do better.

-- | Content regions extracted from a prebuilt Haddock page, links
-- already rewritten for embedding.
data PrebuiltDoc = PrebuiltDoc
  { pdPkgVer      :: !Text
    -- ^ @\<pkg\>-\<ver\>@, for \"open raw haddock\" links.
  , pdDescription :: !(Maybe Text)
    -- ^ Module prose (inner HTML of Haddock's @#description@).
  , pdInterface   :: !Text
    -- ^ The declarations (inner HTML of Haddock's @#interface@).
  , pdContents    :: !(Maybe Text)
    -- ^ Haddock's own contents list, feeds the TOC rail.
  }

-- | Batch-extracted source documentation for one module.
data SourceDoc = SourceDoc
  { sdInfo       :: !Extract.ModuleDocInfo
    -- ^ Header prose + entries, already export-filtered and ordered.
  , sdRawHaddock :: !(Maybe Text)
    -- ^ @Just \<pkg-ver\>@ when a raw prebuilt page also exists (the
    -- module chose source rendering only because content extraction
    -- failed) — still worth an \"open raw haddock\" link.
  }

-- | Everything the symbol card renders.  Replaces the anonymous
-- 5-tuple that used to travel through 'scSymbolLookup'.
data SymbolCardData = SymbolCardData
  { scdSignature  :: !(Maybe Text)
    -- ^ 'Nothing' when the source declares no signature.  Previously the
    -- empty string, which the UI rendered as a blank box and which could
    -- not be told apart from \"we could not read the module\".
  , scdHaddock    :: !(Maybe Text)
    -- ^ Raw comment text; rendered by the UI layer.
  , scdModule     :: !Text
    -- ^ The module that defines the symbol, as resolved — never derived
    -- from a file path.
  , scdRequested  :: !Text
    -- ^ The module the URL asked for.  Kept alongside 'scdModule' so the
    -- card can say \"re-exported by X, defined in Y\" instead of silently
    -- swapping one for the other.
  , scdProvenance :: !Provenance
    -- ^ Whether the definition was resolved or guessed by a package
    -- sweep.  A guess that renders like a fact is worse than no answer.
  , scdLine       :: !(Maybe Int)
    -- ^ Source line when a faithful anchor exists.
  , scdKind       :: !(Maybe DeclKind)
    -- ^ Declaration kind when the parser could classify it.
  }
