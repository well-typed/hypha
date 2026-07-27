{-# LANGUAGE OverloadedStrings #-}
-- | Row constructors for the cache tests.
--
-- The suites that predate definition-site tracking only care about the
-- four original columns, so 'row' fills the other two the way a local
-- declaration would: defined here, publicly exposed.
module Util.Row
  ( row
  , rowIn
  ) where

import Data.Text (Text)

import Hypha.Search.Index (IndexRow (..), Visibility (..))
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))

-- | A locally-declared, exposed row.
row :: Text -> Text -> Text -> Text -> IndexRow
row comp modPath name sig = rowIn comp modPath name sig modPath Exposed

-- | A row with an explicit definition module and visibility.
rowIn :: Text -> Text -> Text -> Text -> Text -> Visibility -> IndexRow
rowIn comp modPath name sig defMod vis = IndexRow
  { rowComponent  = ComponentKey comp
  , rowModule     = ModulePath modPath
  , rowName       = SymbolName name
  , rowSignature  = Signature sig
  , rowDefModule  = ModulePath defMod
  , rowVisibility = vis
  }
