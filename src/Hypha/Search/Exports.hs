{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | What the components indexed so far export, so a component's
-- cross-package re-exports have something to resolve against.
--
-- "Hypha.Search.Reexport" answers /within/ a component: a name no module
-- of the component declares resolves to 'DefinedOutside', naming the
-- import believed to supply it.  Until this module existed the indexer
-- discarded those, which is why @base@ — since GHC 9.10 almost entirely a
-- facade over @ghc-internal@ — contributed 308 rows where @ghc-internal@
-- contributed 1240, and why searching for @mapAccumL@ never found it in
-- @base@.
--
-- Built from 'IndexRow's rather than from parse trees, which buys
-- transitivity for nothing: a dependency's rows already carry their own
-- resolved 'DefinitionRef', so a chain through two packages lands on the
-- real definition without this module searching for it.
module Hypha.Search.Exports
  ( Export (..)
  , ExportChoice (..)
  , ExportEnv
  , emptyEnv
  , extendEnv
  , lookupExport
  ) where

import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import Hypha.Search.Index (DefinitionRef (..), IndexRow (..))
import Hypha.Types.ComponentName
  ( ComponentKey (..), ComponentName (..), parseComponentName )
import Hypha.Types.PackageId (PackageName)
import Hypha.Types.SymbolPath (ModulePath, Signature, SymbolName)

-- | One component's answer for a @(module, name)@ pair.
data Export = Export
  { exPresenter  :: !ComponentKey
    -- ^ The component whose module the asking component names in its
    -- import.  Distinct from the definition's component whenever the
    -- presenter is itself a facade, which is the ordinary case: 'base'
    -- presents @Data.Foldable.foldl'@ whose definition lives in
    -- @ghc-internal@.
  , exDefinition :: !DefinitionRef
  , exSignature  :: !Signature
  }
  deriving stock (Show, Eq)

-- | The chosen answer, plus the candidates it beat.
--
-- A choice among several is not a failure — the caller gets a usable
-- 'Export' either way — but it is worth reporting, so the rejected
-- candidates travel out rather than being discarded at the point of
-- choice.
data ExportChoice = ExportChoice
  { ecChosen   :: !Export
  , ecRejected :: ![DefinitionRef]   -- ^ empty when the choice was forced
  }
  deriving stock (Show, Eq)

-- | Every @(module, name)@ pair the indexed components present.
--
-- The value is a 'NonEmpty' because two components can expose a module of
-- the same name.  Collapsing that to one at insertion time would silently
-- pick a winner for a question only the /asking/ component can answer,
-- which is what 'lookupExport' is for.
newtype ExportEnv = ExportEnv (Map (ModulePath, SymbolName) (NonEmpty Export))
  deriving stock (Show, Eq)

emptyEnv :: ExportEnv
emptyEnv = ExportEnv Map.empty

-- | Add one component's rows.  Called once per component per pass, so the
-- 'NonEmpty' lists grow with the number of components exposing a module
-- name, not with the number of times the pass runs.
extendEnv :: [IndexRow] -> ExportEnv -> ExportEnv
extendEnv rows (ExportEnv env) = ExportEnv (Map.unionWith (<>) added env)
  where
    added = Map.fromListWith (<>)
      [ ((rowModule r, rowName r), exportOf r :| []) | r <- rows ]

    exportOf r = Export
      { exPresenter  = rowComponent r
      , exDefinition = rowDefinition r
      , exSignature  = rowSignature r
      }

-- | The export a component should resolve @(module, name)@ to, given the
-- packages it may resolve through.
--
-- The dependency filter is what keeps two unrelated packages exposing a
-- module of the same name from resolving against each other.  A package
-- may also re-export from its own sub-libraries, so the caller is expected
-- to include the asking unit's own package name in the set.
--
-- The filter tests the /presenting/ component, not the definition's.  The
-- asking module names a module of a direct dependency; where that
-- dependency's own re-export chain ends is none of its business, and
-- filtering on the definition made a facade over a facade unresolvable.
-- Since GHC 9.10 that is the common case rather than an exotic one: a
-- package re-exporting @Data.Foldable.foldl'@ from @base@ would be told
-- the definition lives in @ghc-internal@, which it does not depend on,
-- and the row would be dropped.  Widening the dependency set to its
-- transitive closure would have resolved it too, and would have thrown
-- away the guard: the point is that the /import/ names a direct
-- dependency.
--
-- Ties break lexicographically on the component, so the winner never
-- depends on the order components were indexed in.
lookupExport
  :: Set PackageName
  -> ModulePath
  -> SymbolName
  -> ExportEnv
  -> Maybe ExportChoice
lookupExport deps m n (ExportEnv env) = do
  candidates <- Map.lookup (m, n) env
  case sortOn (unComponentKey . drComponent . exDefinition)
         (NE.filter fromDep candidates) of
    []       -> Nothing
    (e : es) -> Just ExportChoice
      { ecChosen   = e
      , ecRejected = map exDefinition es
      }
  where
    fromDep e = packageOf (exPresenter e) `Set.member` deps
    packageOf = cnPackage . parseComponentName . unComponentKey
