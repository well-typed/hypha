{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | MCP tool catalogue.
--
-- Exposes one MCP tool per @hypha@ CLI subcommand, plus the generic
-- @hypha.exec@ escape hatch.  Each tool is a thin schema wrapper that
-- maps structured JSON arguments to an @argv@ array, which the
-- caller then feeds to the @hypha@ binary.
--
-- The split between schema declaration ('allTools') and argument
-- translation ('argvForTool') keeps the dispatcher in
-- "Hypha.Mcp.Server" free of per-command logic.
module Hypha.Mcp.Tools
  ( -- * Catalogue
    allTools
    -- * Argv translation
  , argvForTool
  ) where

import Data.Aeson
  ( Value (..)
  , object, (.=)
  )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (toList)

-- ---------------------------------------------------------------------------
-- * Catalogue

-- | All MCP tools exposed by @hypha-mcp@.
allTools :: [Value]
allTools =
  [ lookupTool
  , packageTool
  , moduleTool
  , symbolTool
  , sourceTool
  , versionsTool
  , depsTool
  , doctorTool
  , execTool
  ]

-- ---------------------------------------------------------------------------
-- * Schema helpers

-- | A property schema entry: @(name, type, description)@.
type Prop = (Text, Text, Text)

-- | Build a tool descriptor object.
toolDescriptor :: Text -> Text -> Value -> Value
toolDescriptor name desc schema = object
  [ "name"        .= name
  , "description" .= desc
  , "inputSchema" .= schema
  ]

-- | Build an @object@ JSON schema from required + optional properties.
schemaWith :: [Prop] -> [Prop] -> Value
schemaWith required optional = object
  [ "type"       .= ("object" :: Text)
  , "properties" .= object [ propPair p | p <- required <> optional ]
  , "required"   .= [ n | (n, _, _) <- required ]
  ]
  where
    propPair (name, ty, desc) =
      Key.fromText name .= object
        [ "type"        .= ty
        , "description" .= desc
        ]

-- | Global flag properties shared by every per-command tool.
hyphaOptionProps :: [Prop]
hyphaOptionProps =
  [ ("projectDir",      "string",  "Override project root.")
  , ("packageOverride", "array",   "List of `PKG=VER` plan overrides.")
  , ("offline",         "boolean", "Skip network calls.")
  , ("human",           "boolean", "Emit ANSI prose instead of JSON.")
  , ("prettyJson",      "boolean", "Indent JSON output (debugging only).")
  , ("full",            "boolean", "Include all JSON fields (default: compact).")
  , ("select",          "string",  "Comma-separated list of fields to keep.")
  , ("quiet",           "boolean", "Suppress informational output.")
  , ("verbose",         "boolean", "Show debug output.")
  ]

commonOptionsDoc :: Text
commonOptionsDoc = Text.unlines
  [ "Hypha options can be passed via the corresponding fields:"
  , "  projectDir, packageOverride[], offline, human, prettyJson,"
  , "  full, select, quiet, verbose."
  ]

-- ---------------------------------------------------------------------------
-- * Tool definitions

lookupTool :: Value
lookupTool = toolDescriptor "hypha.lookup"
  (Text.unlines
    [ "Tiered Haskell symbol/type lookup against the current cabal"
    , "build plan (cache -> local Hoogle -> remote Hoogle)."
    , ""
    , "`query` may be a name (`filterM`, `Data.Map.lookup`) or a type"
    , "signature (`a -> Maybe a`)."
    , ""
    , commonOptionsDoc
    ])
  (schemaWith
    [ ("query", "string", "Symbol name or type signature to look up.") ]
    hyphaOptionProps)

packageTool :: Value
packageTool = toolDescriptor "hypha.package"
  (Text.unlines
    [ "Package metadata (latest, deprecation, license, exposed modules)."
    , "`pkg` may include a `-version` suffix, e.g. `aeson-2.2.2.0`."
    , ""
    , commonOptionsDoc
    ])
  (schemaWith
    [ ("pkg", "string", "Package id, optionally `pkg-version`.") ]
    hyphaOptionProps)

moduleTool :: Value
moduleTool = toolDescriptor "hypha.module"
  (Text.unlines
    [ "List a module's exported symbols with their signatures."
    , "`path` is `<pkg>/<Module.Path>`, e.g. `async/Control.Concurrent.Async`."
    , ""
    , commonOptionsDoc
    ])
  (schemaWith
    [ ("path", "string", "Module path `<pkg>/<Module>`.") ]
    hyphaOptionProps)

symbolTool :: Value
symbolTool = toolDescriptor "hypha.symbol"
  (Text.unlines
    [ "Signature + Haddock + source coordinates for a single symbol."
    , "`path` is `<pkg>/<Module>/<symbol>`."
    , ""
    , commonOptionsDoc
    ])
  (schemaWith
    [ ("path", "string", "Symbol path `<pkg>/<Module>/<symbol>`.") ]
    hyphaOptionProps)

sourceTool :: Value
sourceTool = toolDescriptor "hypha.source"
  (Text.unlines
    [ "Source slice at the canonical declaration site.  `path` can be"
    , "a symbol (`<pkg>/<Module>/<symbol>`) or a whole module"
    , "(`<pkg>/<Module>`)."
    , ""
    , commonOptionsDoc
    ])
  (schemaWith
    [ ("path", "string", "Symbol or module path.") ]
    hyphaOptionProps)

versionsTool :: Value
versionsTool = toolDescriptor "hypha.versions"
  (Text.unlines
    [ "Version history of a package on Hackage, with the plan-pinned"
    , "version marked."
    , ""
    , commonOptionsDoc
    ])
  (schemaWith
    [ ("pkg", "string", "Package name (no version).") ]
    hyphaOptionProps)

depsTool :: Value
depsTool = toolDescriptor "hypha.deps"
  (Text.unlines
    [ "Forward or reverse dependencies of a package within the plan."
    , ""
    , commonOptionsDoc
    ])
  (schemaWith
    [ ("pkg", "string", "Package name.") ]
    ( [ ("reverse", "boolean", "If true, list reverse deps.")
      , ("depth",   "integer", "Maximum traversal depth.")
      ]
      <> hyphaOptionProps
    ))

doctorTool :: Value
doctorTool = toolDescriptor "hypha.doctor"
  (Text.unlines
    [ "Environment health check (plan.json, cabal store, Hoogle DB,"
    , "external tools)."
    , ""
    , commonOptionsDoc
    ])
  (schemaWith [] hyphaOptionProps)

execTool :: Value
execTool = toolDescriptor "hypha.exec"
  (Text.unlines
    [ "Escape hatch: run an arbitrary `hypha` invocation by passing"
    , "the full argv array.  Prefer the per-subcommand tools"
    , "(`hypha.lookup`, `hypha.symbol`, etc.) where they fit; reach"
    , "for this only when no structured tool matches."
    , ""
    , "Examples:"
    , "  {\"args\": [\"lookup\", \"filterM\"]}"
    , "  {\"args\": [\"symbol\", \"aeson/Data.Aeson/encode\","
    , "             \"--select\", \"sig,haddock\"]}"
    ])
  (object
    [ "type"       .= ("object" :: Text)
    , "properties" .= object
        [ "args" .= object
            [ "type"        .= ("array" :: Text)
            , "items"       .= object [ "type" .= ("string" :: Text) ]
            , "description" .= ("Argv array passed to the hypha binary." :: Text)
            ]
        ]
    , "required" .= ([ "args" ] :: [Text])
    ])

-- ---------------------------------------------------------------------------
-- * Argv translation

-- | Convert structured tool arguments to a @hypha@ argv array.
--
-- Returns @Left@ if the tool name is unknown, a required field is
-- missing, or a field has the wrong JSON shape.
argvForTool :: Text -> Value -> Either Text [Text]
argvForTool name params = case name of
  "hypha.exec"     -> execArgv params
  "hypha.lookup"   -> withGlobals params $ \obj -> do
    q <- requireText obj "query"
    pure ["lookup", q]
  "hypha.package"  -> withGlobals params $ \obj -> do
    p <- requireText obj "pkg"
    pure ["package", p]
  "hypha.module"   -> withGlobals params $ \obj -> do
    p <- requireText obj "path"
    pure ["module", p]
  "hypha.symbol"   -> withGlobals params $ \obj -> do
    p <- requireText obj "path"
    pure ["symbol", p]
  "hypha.source"   -> withGlobals params $ \obj -> do
    p <- requireText obj "path"
    pure ["source", p]
  "hypha.versions" -> withGlobals params $ \obj -> do
    p <- requireText obj "pkg"
    pure ["versions", p]
  "hypha.deps"     -> withGlobals params $ \obj -> do
    p   <- requireText obj "pkg"
    rev <- optBool obj "reverse"
    dep <- optInt  obj "depth"
    pure $ ["deps", p]
        <> (if rev == Just True then ["--reverse"] else [])
        <> maybe [] (\n -> ["--depth", Text.pack (show n)]) dep
  "hypha.doctor"   -> withGlobals params $ \_ -> pure ["doctor"]
  other            -> Left ("Unknown tool: " <> other)

-- | @hypha.exec@: pull argv straight from @{"args": [...]}@.
execArgv :: Value -> Either Text [Text]
execArgv = \case
  Object o -> case KM.lookup (Key.fromText "args") o of
    Just (Array arr) -> Right [ t | String t <- toList arr ]
    Just _           -> Left "`args` must be an array of strings"
    Nothing          -> Left "missing required field `args`"
  _ -> Left "tool arguments must be a JSON object"

-- | Run a per-command argv builder, then prepend global flags.
--
-- Global flags are emitted /before/ the subcommand so that
-- optparse-applicative consumes them at the top-level parser.
withGlobals
  :: Value
  -> (KM.KeyMap Value -> Either Text [Text])
  -> Either Text [Text]
withGlobals params build = case params of
  Object o -> do
    cmd     <- build o
    globals <- globalFlagArgv o
    pure (globals <> cmd)
  _ -> Left "tool arguments must be a JSON object"

-- | Translate the global-flag fields in a params object into argv.
globalFlagArgv :: KM.KeyMap Value -> Either Text [Text]
globalFlagArgv o = do
  projectDir <- optText o "projectDir"
  overrides  <- optTextArray o "packageOverride"
  offline    <- optBool o "offline"
  human      <- optBool o "human"
  prettyJson <- optBool o "prettyJson"
  full       <- optBool o "full"
  sel        <- optText o "select"
  quiet      <- optBool o "quiet"
  verbose    <- optBool o "verbose"
  pure $ concat
    [ maybe [] (\d -> ["--project-dir", d]) projectDir
    , concatMap (\pv -> ["--package-override", pv]) overrides
    , flag offline    "--offline"
    , flag human      "--human"
    , flag prettyJson "--pretty-json"
    , flag full       "--full"
    , maybe [] (\s -> ["--select", s]) sel
    , flag quiet      "--quiet"
    , flag verbose    "--verbose"
    ]
  where
    flag (Just True) name = [name]
    flag _           _    = []

-- ---------------------------------------------------------------------------
-- * Field extractors

requireText :: KM.KeyMap Value -> Text -> Either Text Text
requireText o k = case KM.lookup (Key.fromText k) o of
  Just (String t) -> Right t
  Just _          -> Left ("`" <> k <> "` must be a string")
  Nothing         -> Left ("missing required field `" <> k <> "`")

optText :: KM.KeyMap Value -> Text -> Either Text (Maybe Text)
optText o k = case KM.lookup (Key.fromText k) o of
  Nothing         -> Right Nothing
  Just Null       -> Right Nothing
  Just (String t) -> Right (Just t)
  Just _          -> Left ("`" <> k <> "` must be a string")

optBool :: KM.KeyMap Value -> Text -> Either Text (Maybe Bool)
optBool o k = case KM.lookup (Key.fromText k) o of
  Nothing       -> Right Nothing
  Just Null     -> Right Nothing
  Just (Bool b) -> Right (Just b)
  Just _        -> Left ("`" <> k <> "` must be a boolean")

optInt :: KM.KeyMap Value -> Text -> Either Text (Maybe Int)
optInt o k = case KM.lookup (Key.fromText k) o of
  Nothing         -> Right Nothing
  Just Null       -> Right Nothing
  Just (Number n) -> case toBoundedInteger n of
    Just i  -> Right (Just i)
    Nothing -> Left ("`" <> k <> "` must be an integer in Int range")
  Just _          -> Left ("`" <> k <> "` must be an integer")

optTextArray :: KM.KeyMap Value -> Text -> Either Text [Text]
optTextArray o k = case KM.lookup (Key.fromText k) o of
  Nothing        -> Right []
  Just Null      -> Right []
  Just (Array a) -> traverse asText (toList a)
    where
      asText (String t) = Right t
      asText _          = Left ("`" <> k <> "` must be an array of strings")
  Just _         -> Left ("`" <> k <> "` must be an array of strings")
