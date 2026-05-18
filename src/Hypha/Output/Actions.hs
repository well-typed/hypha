module Hypha.Output.Actions
  ( viewSourceAction
  , moduleIndexAction
  , packageInfoAction
  , reverseDepsAction
  , versionHistoryAction
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | @("view_source", "hypha source <path>")@
viewSourceAction :: Text -> (Text, Text)
viewSourceAction path = (Text.pack "view_source", Text.pack "hypha source " <> path)

-- | @("module_index", "hypha module <mod>")@
moduleIndexAction :: Text -> (Text, Text)
moduleIndexAction modPath = (Text.pack "module_index", Text.pack "hypha module " <> modPath)

-- | @("package_info", "hypha package <pkg>")@
packageInfoAction :: Text -> (Text, Text)
packageInfoAction pkg = (Text.pack "package_info", Text.pack "hypha package " <> pkg)

-- | @("reverse_deps", "hypha deps <pkg> --reverse")@
reverseDepsAction :: Text -> (Text, Text)
reverseDepsAction pkg = (Text.pack "reverse_deps", Text.pack "hypha deps " <> pkg <> Text.pack " --reverse")

-- | @("version_history", "hypha versions <pkg>")@
versionHistoryAction :: Text -> (Text, Text)
versionHistoryAction pkg = (Text.pack "version_history", Text.pack "hypha versions " <> pkg)
