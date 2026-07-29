{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
-- | Resolve the language extensions a module is parsed under.
--
-- The previous approach was a hand-written whitelist inside
-- "Hypha.Source.Parser".  It failed the moment a module used syntax
-- nobody had thought to add: @containers@' @Data.Map.Internal@ carries
-- @type role Map nominal representational@, so the whole module — and
-- with it every symbol it defines — was unreadable.
--
-- A curated list is the wrong shape for this problem.  A module states
-- its own requirements in @{-# LANGUAGE #-}@ pragmas, its component
-- states shared ones in @default-extensions@, and GHC already ships the
-- authoritative flag-name table.  We read all three instead of
-- guessing, so the frontier we support is the frontier
-- @ghc-lib-parser@ supports.
module Hypha.Source.Extensions
  ( LanguageSettings (..)
  , defaultLanguageSettings
  , UnknownExtension (..)
  , extensionFromFlagName
  , resolveExtensions
  , PragmaScan (..)
  , scanPragmas
  , supportedExtensionNames
  , parserOptsFor
  , renderDiagnostics
  ) where

import Control.Exception (evaluate)
import Control.Exception.Safe (SomeException, displayException, try)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (partitionEithers)
-- Qualified because base only re-exports @foldl'@ from Prelude at 4.20+,
-- and we still build against GHC 9.6 (base 4.18) where it does not.
import Data.Foldable qualified as Foldable
import Data.List (uncons)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import GHC.Data.Bag qualified as Bag
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.StringBuffer qualified as SB
import GHC.Driver.Session
  ( Language (..), flagSpecFlag, flagSpecName, languageExtensions
  , supportedLanguagesAndExtensions, xFlags )
import GHC.LanguageExtensions.Type (Extension)
import GHC.Platform.ArchOS (Arch (..), ArchOS (..), OS (..))
import GHC.Parser.Header qualified as Header
import GHC.Parser.Lexer qualified as L
import GHC.Types.Error
  ( Diagnostic (..), Messages, defaultDiagnosticOpts, errMsgDiagnostic
  , getMessages, unDecorated )
import GHC.Types.SrcLoc (unLoc)
import GHC.Utils.Error (emptyDiagOpts)
import GHC.Utils.Outputable (showPprUnsafe)

-- | Language settings a component's cabal stanza fixes for every module
-- in it.  Split into on\/off lists because @default-extensions@ admits
-- @NoImplicitPrelude@ alongside @OverloadedStrings@.
data LanguageSettings = LanguageSettings
  { lsLanguage   :: !(Maybe Language)
    -- ^ @default-language@, when the stanza names one.
  , lsDefaultOn  :: ![Extension]
  , lsDefaultOff :: ![Extension]
  }
  deriving stock (Show, Eq)

-- | No cabal information: the GHC2021 floor alone.  Used for sources we
-- read outside a component context (the CLI's ad-hoc snippet paths).
defaultLanguageSettings :: LanguageSettings
defaultLanguageSettings = LanguageSettings
  { lsLanguage   = Nothing
  , lsDefaultOn  = []
  , lsDefaultOff = []
  }

-- | A @-X@ name GHC's own table does not know.  Carried out of
-- resolution so the caller can report it; never dropped.
newtype UnknownExtension = UnknownExtension { unUnknownExtension :: Text }
  deriving stock (Show, Eq)

-- | GHC's flag-name table, indexed for lookup.  Built once.
--
-- This is why we do not derive names from 'show': @RecordPuns@ is an
-- accepted spelling of the @NamedFieldPuns@ extension and @Rank2Types@
-- of @RankNTypes@, and a table built from the constructor names knows
-- neither alias.  'xFlags' is the same table GHC's own command-line
-- parser consults, so every spelling GHC accepts we accept.
flagTable :: Map.Map Text Extension
flagTable = Map.fromList
  [ (Text.pack (flagSpecName f), flagSpecFlag f) | f <- xFlags ]

-- | Language names accepted in a @LANGUAGE@ pragma, expanded to the
-- extension set they imply.
languageTable :: Map.Map Text Language
languageTable = Map.fromList
  [ ("Haskell98",   Haskell98)
  , ("Haskell2010", Haskell2010)
  , ("GHC2021",     GHC2021)
  , ("GHC2024",     GHC2024)
  ]

-- | Names GHC accepts in a @LANGUAGE@ pragma that name no extension.
--
-- The Safe Haskell modes — @Safe@, @Trustworthy@, @Unsafe@ — constrain
-- what a module may import, not what its syntax means, so they are
-- parse-irrelevant.  Derived by difference rather than listed here:
-- 'supportedLanguagesAndExtensions' is languages ++ overlays ++
-- extensions, and 'extensionFlagNames' is that last part, so what
-- remains is the language selectors ('languageTable', matched first
-- below) and the overlays.  GHC keeps the overlays in a table
-- @ghc-lib-parser@ does not export, and a curated list is the shape
-- this module exists to avoid.
parseIrrelevantNames :: Set.Set Text
parseIrrelevantNames =
  Set.difference
    (Set.fromList (map Text.pack (ghcAcceptedNames ++ ["No" <> n | n <- ghcAcceptedNames])))
    (Set.fromList (map Text.pack extensionFlagNames))

-- | Resolve one pragma name to the @(extension, enabled)@ pairs it
-- implies.  A @No@ prefix flips the flag; a language name expands to its
-- whole set; a Safe Haskell mode implies nothing.
extensionFromFlagName :: Text -> Either UnknownExtension [(Extension, Bool)]
extensionFromFlagName raw
  | Just lang <- Map.lookup raw languageTable
  = Right [ (x, True) | x <- languageExtensions (Just lang) ]
  | Just ext <- Map.lookup raw flagTable
  = Right [(ext, True)]
  | Just bare <- Text.stripPrefix "No" raw
  , Just ext  <- Map.lookup bare flagTable
  = Right [(ext, False)]
  | raw `Set.member` parseIrrelevantNames
  = Right []
  | otherwise
  = Left (UnknownExtension raw)

-- | The extension set a module is parsed under, plus every pragma name
-- we could not resolve.
--
-- Order matters and mirrors GHC: the GHC2021 floor (unioned with the
-- component's @default-language@ when it names one), then the
-- component's @default-extensions@, then the module's own pragmas in
-- source order, so a later @No…@ wins.
--
-- GHC2021 is a floor rather than a substitute because we read source, we
-- do not compile it: a wider set can only let us parse more.  Note that
-- it deliberately excludes the extensions that /change/ parses instead
-- of widening them — @MagicHash@, @TemplateHaskell@, @UnboxedTuples@,
-- @Arrows@, @LinearTypes@, @TransformListComp@, @OverloadedRecordDot@ —
-- so those still require an explicit pragma, exactly as they do for the
-- compiler.
resolveExtensions
  :: LanguageSettings
  -> [Text]                    -- ^ pragma names, source order
  -> (EnumSet.EnumSet Extension, [UnknownExtension])
resolveExtensions ls names =
  let floorExts = languageExtensions (Just GHC2021)
                    ++ maybe [] (languageExtensions . Just) (lsLanguage ls)
      base      = [ (x, True)  | x <- floorExts ++ lsDefaultOn ls ]
                    ++ [ (x, False) | x <- lsDefaultOff ls ]
      (unknown, fromPragmas) = partitionResolved names
      applied   = Foldable.foldl' apply EnumSet.empty (base ++ fromPragmas)
  in (applied, unknown)
  where
    apply acc (x, True)  = EnumSet.insert x acc
    apply acc (x, False) = EnumSet.delete x acc

    partitionResolved ns =
      let (bad, good) = partitionEithers (map extensionFromFlagName ns)
      in (bad, concat good)

-- | What one pass over a module's pragma block found.
--
-- 'psDiagnostics' carries GHC's own complaints about the pragma block
-- (an unrecognised @OPTIONS_GHC@ entry, a malformed @LANGUAGE@ list).
-- They travel with the names rather than being dropped on the floor:
-- a pragma GHC could not read is a plausible cause of the parse failure
-- that follows, and the module page has to be able to say so.
data PragmaScan = PragmaScan
  { psExtensionNames :: ![Text]
    -- ^ Names from @{-# LANGUAGE #-}@ and @{-# OPTIONS_GHC -X… #-}@,
    -- in source order.
  , psDiagnostics    :: ![Text]
  }
  deriving stock (Show, Eq)

-- | Scan a module's pragma block.
--
-- Reading pragmas needs a lexer, and configuring the lexer needs the
-- pragmas: we break the cycle the way GHC does, by lexing with the floor
-- set first and re-initialising afterwards.
--
-- Lives in 'IO' because 'Header.getOptions' /throws/ on a @LANGUAGE@
-- name it does not recognise — it is written for a compiler that wants
-- to abort, and we are a reader that wants to keep going.  The exception
-- becomes a diagnostic and the names we did recover are still returned,
-- so one bad pragma costs us that pragma rather than the whole module.
scanPragmas :: FilePath -> Text -> IO PragmaScan
scanPragmas path src = do
  let (msgs, located) = Header.getOptions opts buf path
  (diags, diagFailure) <- forcedPrefix (renderDiagnostics msgs)
  (names, nameFailure) <- forcedPrefix
    [ name
    | opt <- map unLoc located
    , Just name <- [Text.stripPrefix "-X" (Text.pack opt)]
    ]
  pure PragmaScan
    { psExtensionNames = names
    , psDiagnostics    = diags ++ maybeToList diagFailure
                               ++ maybeToList nameFailure
    }
  where
    (floorExts, _) = resolveExtensions defaultLanguageSettings []
    opts = parserOptsFor floorExts
    buf  = SB.stringToStringBuffer (Text.unpack src)

-- | Force a lazily produced list one cell at a time, stopping at the
-- first cell that throws and reporting why.
--
-- 'Header.getOptions' builds its result with @fmap (args ++)@ over a
-- recursive walk of the token stream, and validates each @LANGUAGE@ name
-- as that walk reaches it.  So an unrecognised name throws from /inside/
-- the list — forcing the whole list at once would lose the names before
-- it too, which is the difference between one bad pragma costing us that
-- pragma and it costing us the module.
forcedPrefix :: [Text] -> IO ([Text], Maybe Text)
forcedPrefix = go []
  where
    go acc xs = do
      -- 'uncons' forces the cell, not the element: the throw can come
      -- from either, so both are attempted separately.
      cell <- try (evaluate (uncons xs))
      case cell of
        Left e                  -> pure (reverse acc, Just (describe e))
        Right Nothing           -> pure (reverse acc, Nothing)
        Right (Just (y, rest))  -> do
          forced <- try (evaluate (Text.length y))
          case forced of
            Left e  -> pure (reverse acc, Just (describe e))
            Right _ -> go (y : acc) rest

    describe :: SomeException -> Text
    describe = firstLine . Text.pack . displayException

    firstLine = Text.strip . Text.takeWhile (/= '\n')

-- | Render GHC diagnostics to plain lines.  Shared with
-- "Hypha.Source.Parser" so a parse failure and a pragma complaint are
-- rendered by the same code.
renderDiagnostics :: forall e. Diagnostic e => Messages e -> [Text]
renderDiagnostics = map render . Bag.bagToList . getMessages
  where
    -- A diagnostic is a headline plus decorations; join them into one
    -- line each so callers can trace or attach them without knowing
    -- anything about GHC's pretty-printer.
    render msg = Text.unwords
      [ Text.pack (showPprUnsafe d)
      | d <- unDecorated
               (diagnosticMessage (defaultDiagnosticOpts @e) (errMsgDiagnostic msg))
      ]

-- | Every name GHC accepts in a @LANGUAGE@ pragma or an @OPTIONS_GHC -X@
-- entry.
--
-- 'Header.getOptions' validates each entry against this list and throws
-- on anything absent, so passing @[]@ (as the whitelist-era code did)
-- rejects every pragma in the language — including @CPP@.
--
-- Union of two sources, because neither alone is complete for a reader.
-- 'xFlags' has every extension but neither the language selectors nor
-- the Safe Haskell modes, and omitting the latter cost us every pragma
-- in 1837 modules of a real build plan: one @{-\# LANGUAGE Trustworthy
-- \#-}@ made 'scanPragmas' throw, and @async@'s @MagicHash@ and
-- @UnboxedTuples@ went down with it.  'supportedLanguagesAndExtensions'
-- has all three groups but filters the extensions by platform — off a
-- JavaScript backend it drops @JavaScriptFFI@, and we still have to read
-- a module that names it — so we take the union and lose no spelling.
supportedExtensionNames :: [String]
supportedExtensionNames = nubOrd (extensionFlagNames ++ ghcAcceptedNames)

-- | Every @-X@ extension name, both polarities, unfiltered by platform.
extensionFlagNames :: [String]
extensionFlagNames =
  concat [ [n, "No" <> n] | n <- map flagSpecName xFlags ]

-- | GHC's own answer to @ghc --supported-extensions@: languages, Safe
-- Haskell overlays and extensions.  The 'ArchOS' only narrows the
-- extension half, which 'extensionFlagNames' supplies in full anyway.
ghcAcceptedNames :: [String]
ghcAcceptedNames = supportedLanguagesAndExtensions (ArchOS ArchUnknown OSUnknown)

-- | The parser options hypha parses under, given a resolved extension
-- set.  Single definition so the pragma-reading pass and the real parse
-- cannot drift apart.
parserOptsFor :: EnumSet.EnumSet Extension -> L.ParserOpts
parserOptsFor exts =
  L.mkParserOpts
    exts
    emptyDiagOpts
    supportedExtensionNames
    False   -- safeImports
    True    -- isHaddock — attach doc comments to the parse tree
    False   -- keep raw token stream
    True    -- honour @{-# LINE #-}@ pragmas
