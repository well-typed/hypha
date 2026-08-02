{-# LANGUAGE OverloadedStrings #-}
-- | Row constructors for the cache tests.
--
-- The suites that predate definition-site tracking only care about the
-- four original columns, so 'row' fills the others the way a local
-- declaration would: defined here, in this component, publicly exposed.
module Util.Row
  ( row
  , rowIn
  , rowFrom
  , envFromRows
  ) where

import Data.Text (Text)

import Hypha.Search.Exports (ExportEnv, emptyEnv, extendEnv)
import Hypha.Search.Index (DefinitionRef (..), IndexRow (..), Visibility (..))
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))

-- | A locally-declared, exposed row.
row :: Text -> Text -> Text -> Text -> IndexRow
row comp modPath name sig = rowIn comp modPath name sig modPath Exposed

-- | A row defined in another module of the /same/ component.
rowIn :: Text -> Text -> Text -> Text -> Text -> Visibility -> IndexRow
rowIn comp modPath name sig defMod =
  rowFrom comp modPath name sig
    (DefinitionRef (ComponentKey comp) (ModulePath defMod))

-- | A row whose definition may live in another component.
rowFrom :: Text -> Text -> Text -> Text -> DefinitionRef -> Visibility -> IndexRow
rowFrom comp modPath name sig def vis = IndexRow
  { rowComponent  = ComponentKey comp
  , rowModule     = ModulePath modPath
  , rowName       = SymbolName name
  , rowSignature  = Signature sig
  , rowDefinition = def
  , rowVisibility = vis
  }

-- | An 'ExportEnv' holding exactly these rows.
--
-- Lives here rather than in the library: 'emptyEnv' and 'extendEnv' are
-- the API, and a one-line composition of them with no production caller
-- was library surface the tests were keeping alive.
envFromRows :: [IndexRow] -> ExportEnv
envFromRows rows = extendEnv rows emptyEnv
