# Hypha Index, Parse and Search Correctness — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `hypha server`'s index, parser and search tell the truth: module names come from the module, language extensions come from its pragmas, and re-exports resolve to a definition site rather than to a same-named symbol.

**Architecture:** Four new modules split the work currently tangled inside `Command/Server.hs` and `Source/{Parser,Locate}.hs`: `Hypha.Source.Extensions` (language settings → extension set), `Hypha.Source.Interface` (one parse → module name, exports, imports, decls), `Hypha.Search.Reexport` (pure definition-site resolution over a component's interfaces), `Hypha.Search.Index` (row types, build, hydrate, cache I/O). Everything downstream — index rows, search ranking, the module page's "On this page" rail, the symbol card — consumes those instead of re-deriving facts from file paths.

**Tech Stack:** GHC 9.10.3 (also 9.6.7 / 9.12.4 project files), `ghc-lib-parser >= 9.10 && < 9.13`, `Cabal` (for `.cabal` parsing), `sqlite-simple`, `lucid`, `servant`, `tasty` + `tasty-hunit` / `tasty-golden` / `falsify`, `cpphs`.

**Spec:** `docs/superpowers/specs/2026-07-27-hypha-index-correctness-design.md`

## Global Constraints

- Strict bangs on every strict field in `data`/`newtype`; a lazy field gets a one-line comment saying why.
- No `error`/`undefined` in production code. Boundary failures go through `Hypha.Error.HyphaError` or a typed sub-error.
- No silent error branches. `Left _ -> pure fallback`, `fromRight`, `either (const x) id` and `_err` bindings are banned; every degraded path traces its cause to stderr or carries it in the value.
- Never round-trip our own output (no `Aeson.decode` of bytes we produced).
- Domain types stay themselves until the rendering edge: `ModulePath`, `SymbolName`, `Signature`, `ComponentKey`, `PackageName`, `Version`, `Visibility`. No `Text.pack . show` into an error or a row.
- Records-of-functions over `m` for effects. No effect library, no typeclass effect machinery.
- `deriving stock` / `deriving newtype`, explicit and sorted imports, `Hypha.Prelude` for shared shorthands.
- Every new module is listed in `hypha.cabal`'s library `exposed-modules`; every new test module in the test-suite's `other-modules` **and** wired into `test/Main.hs`'s `allTests`.
- Build/test command: `cabal build all && cabal test all`. `ghc`/`cabal` live in `~/.ghcup/bin`; the sandbox hides them, so builds need `PATH` set and the sandbox disabled.
- Commit at the end of every task, Conventional Commits, no pushing.
- Git identity is not configured in this environment. Commit with:
  `git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' commit …`

---

## File Structure

**New library modules**

| File | Responsibility |
|---|---|
| `src/Hypha/Source/Extensions.hs` | `LanguageSettings`, pragma extraction, `resolveExtensions`, `UnknownExtension`. |
| `src/Hypha/Source/Interface.hs` | `ModuleInterface`, `ExportItem`, `ImportItem`, `SrcLine`; one parse per module. |
| `src/Hypha/Search/Reexport.hs` | Pure: `[ModuleInterface]` → `Map (ModulePath, SymbolName) (DefinitionSite, Ambiguity)`. |
| `src/Hypha/Search/Index.hs` | `IndexRow`, `Visibility`, build + hydrate, cache round-trip. |

**Modified library modules**

| File | Change |
|---|---|
| `src/Hypha/Types/SymbolPath.hs` | Add `Signature` newtype. |
| `src/Hypha/Types/ComponentName.hs` | Add `ComponentKey` newtype + `componentKey`. |
| `src/Hypha/Source/Parser.hs` | Typed `ParseError`; extensions per module; `ParseContext`. |
| `src/Hypha/Source/Locate.hs` | `scanFile` returns `Either ParseError (Maybe …)`; `LocatedDefinition`; delete `sortByPrefix`/`sortBy`. |
| `src/Hypha/Source/Extract.hs` | `DocEntry` gains `deOrigin`; resolved-entry assembly. |
| `src/Hypha/Project/Components.hs` | `ciOtherModules`, `ciDefaultExtensions`, `ciLanguage`. |
| `src/Hypha/Search/Cache.hs` | `def_mod` / `visibility` columns, `index_format` wipe. |
| `src/Hypha/Search/PackageCache.hs` | `IndexRow` in place of 4-tuples. |
| `src/Hypha/Search/Fuzzy.hs` | `ResultKind` on `IndexedRow`, kind-aware scoring. |
| `src/Hypha/Command/Server.hs` | Indexing moves out; symbol card + module doc use resolved data. |
| `src/Hypha/Server/ModuleDoc.hs` | `SymbolCardData` typed fields; `EntryOrigin`. |
| `src/Hypha/Server/Ui/Search.hs` | Renders `SearchResult`; `resultHref`. |
| `src/Hypha/Server/Ui/Tree.hs` | `hackageLink` origins + newtype params. |
| `src/Hypha/Server/Ui/ModuleDoc.hs` | Re-export provenance in entries and TOC. |
| `src/Hypha/Command/Doctor.hs` | Report index format + parse-failure count. |

**New tests**

| File | Covers |
|---|---|
| `test/Unit/SourceExtensions.hs` | Task 1 |
| `test/Unit/SourceInterface.hs` | Task 4 |
| `test/Unit/SearchReexport.hs` | Task 5 |
| `test/Unit/SearchIndexCache.hs` | Task 7 |
| `test/Unit/SearchIndexBuild.hs` | Task 8 |
| `test/Unit/SearchCollapse.hs` | Task 11 |
| `test/Property/SearchRanking.hs` | Task 10, 11 |
| `test/fixtures/reexport/` | Tasks 4, 5, 8, 12 |

---

### Task 1: `Hypha.Source.Extensions` — language settings, no whitelist

**Files:**
- Create: `src/Hypha/Source/Extensions.hs`
- Create: `test/Unit/SourceExtensions.hs`
- Modify: `hypha.cabal` (library `exposed-modules`, test `other-modules`), `test/Main.hs`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces:
  ```haskell
  data LanguageSettings = LanguageSettings
    { lsLanguage   :: !(Maybe Language)
    , lsDefaultOn  :: ![Extension]
    , lsDefaultOff :: ![Extension]
    }
  defaultLanguageSettings :: LanguageSettings
  newtype UnknownExtension = UnknownExtension { unUnknownExtension :: Text }
  extensionFromFlagName :: Text -> Either UnknownExtension [(Extension, Bool)]
  resolveExtensions :: LanguageSettings -> [Text] -> (EnumSet.EnumSet Extension, [UnknownExtension])
  pragmaExtensionNames :: FilePath -> Text -> [Text]
  ```
  `extensionFromFlagName` returns a *list* of `(Extension, Bool)` because
  `-XGHC2021` / `-XHaskell2010` name whole languages; `Bool` is on/off so
  `NoImplicitPrelude` can turn one off.

- [ ] **Step 1: Write the failing test**

Create `test/Unit/SourceExtensions.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for 'Hypha.Source.Extensions'.  The regression that
-- motivated the module — @Data.Map.Internal@'s @type role@ annotation
-- failing to parse under a hand-written extension whitelist — is the
-- first case in the suite.
module Unit.SourceExtensions (tests) where

import qualified Data.Text as Text

import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.LanguageExtensions as LangExt

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Source.Extensions
  ( LanguageSettings (..), UnknownExtension (..)
  , defaultLanguageSettings, extensionFromFlagName
  , pragmaExtensionNames, resolveExtensions )

tests :: TestTree
tests = testGroup "Unit.SourceExtensions"
  [ testCase "module pragmas enable RoleAnnotations and MagicHash" $ do
      let src = Text.unlines
            [ "{-# LANGUAGE CPP #-}"
            , "{-# LANGUAGE RoleAnnotations #-}"
            , "{-# LANGUAGE MagicHash #-}"
            , "module M where"
            ]
          (exts, unknown) =
            resolveExtensions defaultLanguageSettings (pragmaExtensionNames "M.hs" src)
      unknown @?= []
      assertBool "RoleAnnotations on" (EnumSet.member LangExt.RoleAnnotations exts)
      assertBool "MagicHash on"      (EnumSet.member LangExt.MagicHash exts)

  , testCase "GHC2021 floor is present without any pragma" $ do
      let (exts, _) = resolveExtensions defaultLanguageSettings []
      assertBool "ScopedTypeVariables in GHC2021"
        (EnumSet.member LangExt.ScopedTypeVariables exts)
      assertBool "MagicHash NOT in the floor"
        (not (EnumSet.member LangExt.MagicHash exts))

  , testCase "No- prefix turns an extension off, last pragma wins" $ do
      let (exts, _) = resolveExtensions defaultLanguageSettings
                        ["ScopedTypeVariables", "NoScopedTypeVariables"]
      assertBool "turned off" (not (EnumSet.member LangExt.ScopedTypeVariables exts))

  , testCase "flag aliases resolve through GHC's own table" $ do
      -- Rank2Types is an alias for RankNTypes; NamedFieldPuns is the
      -- flag name for the RecordPuns extension.  A show-based table
      -- would miss both, which is why we use xFlags.
      extensionFromFlagName "Rank2Types"    @?= Right [(LangExt.RankNTypes, True)]
      extensionFromFlagName "NamedFieldPuns" @?= Right [(LangExt.RecordPuns, True)]

  , testCase "language selectors expand to their extension set" $
      case extensionFromFlagName "Haskell2010" of
        Left e   -> fail ("Haskell2010 rejected: " <> show (unUnknownExtension e))
        Right xs -> assertBool "non-empty" (not (null xs))

  , testCase "unknown extension is reported, not dropped" $ do
      let (_, unknown) = resolveExtensions defaultLanguageSettings ["NoSuchExtension"]
      map unUnknownExtension unknown @?= ["NoSuchExtension"]

  , testCase "cabal default-extensions apply below module pragmas" $ do
      let ls = defaultLanguageSettings { lsDefaultOn = [LangExt.MagicHash] }
          (exts, _) = resolveExtensions ls ["NoMagicHash"]
      assertBool "module pragma wins over cabal default"
        (not (EnumSet.member LangExt.MagicHash exts))

  , testCase "OPTIONS_GHC -X pragmas are picked up too" $ do
      let src = Text.unlines
            [ "{-# OPTIONS_GHC -XRoleAnnotations #-}"
            , "module M where"
            ]
      assertBool "RoleAnnotations seen"
        ("RoleAnnotations" `elem` pragmaExtensionNames "M.hs" src)
  ]
```

- [ ] **Step 2: Wire the test module in, run it, verify it fails**

Add `Unit.SourceExtensions` to `hypha.cabal`'s test-suite `other-modules`, add `import qualified Unit.SourceExtensions` and `Unit.SourceExtensions.tests` to `test/Main.hs`.

Run: `cabal test all --test-options='-p Unit.SourceExtensions'`
Expected: FAIL — `Could not find module 'Hypha.Source.Extensions'`.

- [ ] **Step 3: Write the module**

Create `src/Hypha/Source/Extensions.hs`:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
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
  , pragmaExtensionNames
  , parserOptsFor
  ) where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Data.StringBuffer as SB
import GHC.Driver.Flags (FlagSpec (..))
import GHC.Driver.Session (Language (..), languageExtensions, xFlags)
import GHC.LanguageExtensions.Type (Extension)
import qualified GHC.Parser.Header as Header
import qualified GHC.Parser.Lexer as L
import GHC.Types.SrcLoc (unLoc)
import GHC.Utils.Error (emptyDiagOpts)

-- | Language settings a component's cabal stanza fixes for every module
-- in it.  Split into on/off lists because @default-extensions@ admits
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
-- This is why we do not derive names from 'show': the flag
-- @NamedFieldPuns@ names the extension @RecordPuns@, and @Rank2Types@
-- is an alias of @RankNTypes@.  'xFlags' is the same table GHC's
-- command-line parser consults, so aliases come for free.
flagTable :: Map.Map Text Extension
flagTable = Map.fromList
  [ (Text.pack (flagSpecName f), flagSpecFlag f) | f <- xFlags ]

-- | Language names accepted in a @LANGUAGE@ pragma, expanded to the
-- extension set they imply.
languageTable :: Map.Map Text Language
languageTable = Map.fromList
  [ ("Haskell98", Haskell98)
  , ("Haskell2010", Haskell2010)
  , ("GHC2021", GHC2021)
  , ("GHC2024", GHC2024)
  ]

-- | Resolve one pragma name to the @(extension, enabled)@ pairs it
-- implies.  A @No@ prefix flips the flag; a language name expands to
-- its whole set.
extensionFromFlagName :: Text -> Either UnknownExtension [(Extension, Bool)]
extensionFromFlagName raw
  | Just lang <- Map.lookup raw languageTable
  = Right [ (x, True) | x <- languageExtensions (Just lang) ]
  | Just ext <- Map.lookup raw flagTable
  = Right [(ext, True)]
  | Just bare <- Text.stripPrefix "No" raw
  , Just ext  <- Map.lookup bare flagTable
  = Right [(ext, False)]
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
-- @Arrows@, @LinearTypes@, @TransformListComp@,
-- @OverloadedRecordDot@ — so those still require an explicit pragma,
-- exactly as they do for the compiler.
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
      applied   = foldl' apply EnumSet.empty (base ++ fromPragmas)
  in (applied, unknown)
  where
    apply acc (x, True)  = EnumSet.insert x acc
    apply acc (x, False) = enumSetDelete x acc

    partitionResolved = foldl' step ([], [])
      where
        step (bad, good) n = case extensionFromFlagName n of
          Left  e  -> (bad ++ [e], good)
          Right xs -> (bad, good ++ xs)

-- | 'EnumSet' has no delete, so rebuild without the member.  The sets
-- are tiny (bounded by the extension count) and this runs once per
-- module, not per token.
enumSetDelete :: Extension -> EnumSet.EnumSet Extension -> EnumSet.EnumSet Extension
enumSetDelete x =
  EnumSet.fromList . filter (/= x) . EnumSet.toList

-- | Extension names named by the module's own @{-# LANGUAGE #-}@ and
-- @{-# OPTIONS_GHC -X… #-}@ pragmas, in source order.
--
-- Reading pragmas needs a lexer, and configuring the lexer needs the
-- pragmas: we break the cycle the way GHC does, by lexing with the
-- floor set first and re-initialising afterwards.
pragmaExtensionNames :: FilePath -> Text -> [Text]
pragmaExtensionNames path src =
  [ name
  | opt <- map unLoc (Header.getOptions opts buf path)
  , Just name <- [Text.stripPrefix "-X" (Text.pack opt)]
  ]
  where
    (floorExts, _) = resolveExtensions defaultLanguageSettings []
    opts = parserOptsFor floorExts
    buf  = SB.stringToStringBuffer (Text.unpack src)

-- | The parser options hypha parses under, given a resolved extension
-- set.  Single definition so the pragma-reading pass and the real parse
-- cannot drift apart.
parserOptsFor :: EnumSet.EnumSet Extension -> L.ParserOpts
parserOptsFor exts =
  L.mkParserOpts
    exts
    emptyDiagOpts
    []      -- supported langexts (error messages only)
    False   -- safeImports
    True    -- isHaddock — attach doc comments to the parse tree
    False   -- keep raw token stream
    True    -- honour @{-# LINE #-}@ pragmas
```

Add `Hypha.Source.Extensions` to `hypha.cabal`'s library `exposed-modules`.

- [ ] **Step 4: Run the test, iterate on the ghc-lib-parser API**

Run: `cabal test all --test-options='-p Unit.SourceExtensions'`
Expected: PASS.

If `GHC.Parser.Header.getOptions`, `GHC.Driver.Session.xFlags` or `GHC.Driver.Flags.FlagSpec` are not where this code expects them, find them with:

```bash
grep -rl 'getOptions\|xFlags' ~/.cabal/store/*/ghc-lib-parser-9.10*/lib --include='*.hi' 2>/dev/null
```

and adjust the imports. All three modules ship in `ghc-lib-parser-9.10.3.20250912` (verified). Both the argument order of `mkParserOpts` and the `Language` constructors are already used by `Source/Parser.hs`, so copy from there if a signature differs. Keep any shim inside this module — no CPP anywhere else.

- [ ] **Step 5: Verify both other GHCs still build**

Run:
```bash
cabal build all --project-file=cabal.ghc-9.12.4.project
cabal build all --project-file=cabal.ghc-9.6.7.project
```
Expected: both succeed. If `ghc-lib-parser-9.12` moved `getOptions`, add the CPP shim here.

- [ ] **Step 6: Commit**

```bash
git add src/Hypha/Source/Extensions.hs test/Unit/SourceExtensions.hs hypha.cabal test/Main.hs
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "feat(source): resolve language extensions from pragmas and cabal"
```

---

### Task 2: Typed `ParseError`, parser reads the module's pragmas

**Files:**
- Modify: `src/Hypha/Source/Parser.hs:88-90` (`ParseError`), `:118-140` (`parseModuleDocIO`), `:214-240` (delete `enabledExtensions`)
- Modify: `test/Unit/SourceParser.hs`
- Create: `test/fixtures/reexport/src/Fixture/Internal.hs` (first use; grown in Task 4)

**Interfaces:**
- Consumes: `LanguageSettings`, `resolveExtensions`, `pragmaExtensionNames`, `parserOptsFor` (Task 1).
- Produces:
  ```haskell
  data ParseError = ParseError
    { peMessage           :: !Text
    , peLine              :: !(Maybe Int)
    , peUnknownExtensions :: ![UnknownExtension]
    }
  parseErrorMessage :: ParseError -> Text          -- kept: existing callers
  parseDecls        :: FilePath -> Text -> Either ParseError [Decl]
  parseDeclsWith    :: LanguageSettings -> FilePath -> Text -> Either ParseError [Decl]
  parseModuleDoc    :: FilePath -> Text -> Either ParseError (Maybe Text, [Decl])
  parseModuleDocWith :: LanguageSettings -> FilePath -> Text -> Either ParseError (Maybe Text, [Decl])
  ```
  The bare `parseDecls`/`parseModuleDoc` keep their signatures and delegate with `defaultLanguageSettings`, so no caller breaks in this task.

- [ ] **Step 1: Write the failing tests**

Create `test/fixtures/reexport/src/Fixture/Internal.hs`:

```haskell
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
-- | Definition site for the fixture package.  Mirrors the shapes that
-- broke the real indexer: a role annotation, an unboxed primitive, and
-- CPP.
module Fixture.Internal
  ( Bag (..)
  , insertBag
  , sizeBag
  , internalOnly
  ) where

import GHC.Exts (Int (..), (+#))

-- | A bag of values.
data Bag a = Bag ![a]

type role Bag nominal

-- | Insert into the bag.
insertBag :: a -> Bag a -> Bag a
insertBag x (Bag xs) = Bag (x : xs)

-- | Size of the bag, via an unboxed add so MagicHash is load-bearing.
sizeBag :: Bag a -> Int
sizeBag (Bag xs) = case length xs of
  I# n -> I# (n +# 0#)

-- | Not exported by any wrapper; stays Internal-only.
internalOnly :: Bag a -> Bool
internalOnly (Bag xs) = null xs
```

Append to `test/Unit/SourceParser.hs`'s `tests` list:

```haskell
  , testCase "role annotations and MagicHash parse (was: whitelist miss)" $ do
      src <- Text.pack <$> readFile "test/fixtures/reexport/src/Fixture/Internal.hs"
      case parseDecls "Fixture/Internal.hs" src of
        Left e   -> fail ("unexpected parse error: " <> show (parseErrorMessage e))
        Right ds -> do
          let names = map declName ds
          assertBool "insertBag found" ("insertBag" `elem` names)
          assertBool "sizeBag found"   ("sizeBag"   `elem` names)

  , testCase "parse failure carries GHC's message and a line" $ do
      let src = Text.unlines
            [ "module M where"
            , ""
            , "f x = case x of"
            , "  -> 1"
            ]
      case parseDecls "M.hs" src of
        Right _ -> fail "expected a parse error"
        Left e  -> do
          assertBool "message is not the literal 'parse error'"
            (parseErrorMessage e /= "parse error")
          assertBool "message is non-empty"
            (not (Text.null (parseErrorMessage e)))
          peLine e @?= Just 4

  , testCase "unknown pragma name is reported on success" $ do
      let src = Text.unlines
            [ "{-# LANGUAGE NoSuchExtension #-}"
            , "module M where"
            , "f :: Int"
            , "f = 1"
            ]
      case parseModuleDoc "M.hs" src of
        Left e  -> fail ("unexpected parse error: " <> show (parseErrorMessage e))
        Right _ -> pure ()
      -- The names travel on the error path; on the success path the
      -- caller gets them from 'pragmaExtensionNames' + 'resolveExtensions'.
      -- Assert the resolution layer sees it:
      let (_, unknown) = resolveExtensions defaultLanguageSettings
                           (pragmaExtensionNames "M.hs" src)
      map unUnknownExtension unknown @?= ["NoSuchExtension"]
```

Extend the import list of `test/Unit/SourceParser.hs`:

```haskell
import Hypha.Source.Extensions
  ( UnknownExtension (..), defaultLanguageSettings
  , pragmaExtensionNames, resolveExtensions )
import Hypha.Source.Parser
  ( Decl (..), DeclKind (..), ParseError (..), parseDecls, parseModuleDoc
  , parseErrorMessage, findDecl, declSigText )
```

- [ ] **Step 2: Run the tests, verify they fail**

Run: `cabal test all --test-options='-p Unit.SourceParser'`
Expected: FAIL — the fixture case fails with a parse error (missing `RoleAnnotations`), the message case fails on `parseErrorMessage e /= "parse error"`, `peLine` does not exist.

- [ ] **Step 3: Make `ParseError` a record**

In `src/Hypha/Source/Parser.hs`, replace lines 88-90:

```haskell
-- | Carrier for any parser failure surfaced from @ghc-lib-parser@.
--
-- The message is GHC's own rendered diagnostic: the module page shows
-- this string to the user, and \"parse error\" (all we used to carry)
-- told them nothing.  'peUnknownExtensions' lists pragma names GHC's
-- flag table did not recognise, which is a plausible cause of the
-- failure and must not be swallowed.
data ParseError = ParseError
  { peMessage           :: !Text
  , peLine              :: !(Maybe Int)
  , peUnknownExtensions :: ![Extensions.UnknownExtension]
  }
  deriving stock (Show, Eq)

-- | Backwards-compatible accessor: existing callers render this.
parseErrorMessage :: ParseError -> Text
parseErrorMessage = peMessage
```

Export `ParseError (..)` and `parseErrorMessage` from the module header.

- [ ] **Step 4: Resolve extensions per module and render the diagnostic**

Replace `parseModuleDocIO` (lines ~121-139) and delete `enabledExtensions` (lines ~214-240):

```haskell
parseModuleDocIO
  :: Extensions.LanguageSettings
  -> FilePath
  -> Text
  -> IO (Either ParseError (Maybe Text, [Decl]))
parseModuleDocIO ls path source = do
  preprocessed <- if needsCpp source
    then Text.pack <$> Cpphs.runCpphs cpphsOpts path (Text.unpack source)
    else pure source
  let (exts, unknown) =
        Extensions.resolveExtensions ls
          (Extensions.pragmaExtensionNames path preprocessed)
      buf  = SB.stringToStringBuffer (Text.unpack preprocessed)
      loc  = mkRealSrcLoc (mkFastString path) 1 1
      st   = L.initParserState (Extensions.parserOptsFor exts) buf loc
  pure $ case L.unP P.parseModule st of
    L.POk _ (L _ hsMod) -> Right (moduleHeaderDoc hsMod, declsFromModule hsMod)
    L.PFailed st'       -> Left (parseFailure unknown st')

-- | Turn a failed parser state into our typed error.  GHC's own
-- diagnostics are rendered rather than discarded, so the module page
-- can say what went wrong and where.
parseFailure :: [Extensions.UnknownExtension] -> L.PState -> ParseError
parseFailure unknown st =
  let msgs   = L.getPsErrorMessages st
      firstD = listToMaybe (Bag.bagToList (getMessages msgs))
  in ParseError
       { peMessage = case firstD of
           Nothing -> "parse error"
           Just d  -> Text.pack (showPprUnsafe (errMsgDiagnostic d))
       , peLine = case firstD of
           Just d | RealSrcSpan s _ <- errMsgSpan d -> Just (srcSpanStartLine s)
           _                                        -> Nothing
       , peUnknownExtensions = unknown
       }
```

Add the imports this needs (`GHC.Data.Bag qualified as Bag`, `GHC.Types.Error (getMessages, errMsgDiagnostic, errMsgSpan)`, `GHC.Utils.Outputable (showPprUnsafe)`, `Hypha.Source.Extensions qualified as Extensions`), and keep the pure wrappers:

```haskell
parseDecls :: FilePath -> Text -> Either ParseError [Decl]
parseDecls = parseDeclsWith Extensions.defaultLanguageSettings

parseDeclsWith :: Extensions.LanguageSettings -> FilePath -> Text -> Either ParseError [Decl]
parseDeclsWith ls path source =
  unsafePerformIO (fmap (fmap snd) (parseModuleDocIO ls path source))
{-# NOINLINE parseDeclsWith #-}

parseModuleDoc :: FilePath -> Text -> Either ParseError (Maybe Text, [Decl])
parseModuleDoc = parseModuleDocWith Extensions.defaultLanguageSettings

parseModuleDocWith
  :: Extensions.LanguageSettings -> FilePath -> Text
  -> Either ParseError (Maybe Text, [Decl])
parseModuleDocWith ls path source = unsafePerformIO (parseModuleDocIO ls path source)
{-# NOINLINE parseModuleDocWith #-}
```

Update `parseDeclsIO` to take `LanguageSettings` and fix its callers (`grep -rn 'parseDeclsIO' src test`).

- [ ] **Step 5: Run the tests**

Run: `cabal test all --test-options='-p Unit.SourceParser'`
Expected: PASS. If GHC's error-rendering API differs, find the accessors:

```bash
grep -rn 'getPsErrorMessages\|errMsgDiagnostic' ~/.cabal/store/*/ghc-lib-parser-9.10*/lib --include='*.hi' 2>/dev/null | head
```

- [ ] **Step 6: Full suite, then commit**

Run: `cabal build all && cabal test all`
Expected: PASS. `Golden/Source.hs` and `Unit/SourceExtract.hs` exercise the old whitelist path; if a golden file changes because a module now parses that previously did not, inspect the diff and accept it only when the new output is *more* complete.

```bash
git add src/Hypha/Source/Parser.hs test/Unit/SourceParser.hs test/fixtures/reexport
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "fix(source): parse under the module's own extensions, keep GHC's diagnostic"
```

---

### Task 3: Cabal stanza gains modules, extensions and language

**Files:**
- Modify: `src/Hypha/Project/Components.hs:44-52` (`ComponentInfo`), `:96-106` (`toComponent`)
- Modify: `test/Unit/Components.hs`
- Modify: `test/fixtures/cabal/` (add a fixture cabal file with the new fields)

**Interfaces:**
- Consumes: `LanguageSettings` (Task 1).
- Produces:
  ```haskell
  data ComponentInfo = ComponentInfo
    { ciKind             :: !ComponentKind
    , ciHsSourceDirs     :: ![FilePath]
    , ciExposedModules   :: ![Text]
    , ciOtherModules     :: ![Text]
    , ciLanguageSettings :: !LanguageSettings
    }
  ```
  One `LanguageSettings` field rather than three loose ones, so the value
  the parser wants travels whole.

- [ ] **Step 1: Write the failing test**

Create `test/fixtures/cabal/extensions.cabal`:

```cabal
cabal-version: 2.4
name:          extensions-fixture
version:       0.1.0

library
  hs-source-dirs:     src
  exposed-modules:    Fixture.Wrapper
  other-modules:      Fixture.Internal
  default-language:   GHC2021
  default-extensions: MagicHash
                    , NoImplicitPrelude
  build-depends:      base
```

Append to `test/Unit/Components.hs`:

```haskell
  , testCase "cabal other-modules, default-extensions and language are read" $ do
      comps <- parseLibComponents "test/fixtures/cabal/extensions.cabal" "/pkg"
      case comps of
        [c] -> do
          ciExposedModules c @?= ["Fixture.Wrapper"]
          ciOtherModules   c @?= ["Fixture.Internal"]
          let ls = ciLanguageSettings c
          lsLanguage   ls @?= Just GHC2021
          lsDefaultOn  ls @?= [LangExt.MagicHash]
          lsDefaultOff ls @?= [LangExt.ImplicitPrelude]
        _ -> fail ("expected exactly one component, got " <> show (length comps))
```

with imports `Distribution.PackageDescription (Language (..))` — note cabal's own `Language` is *not* GHC's; see Step 3 — plus `qualified GHC.LanguageExtensions as LangExt` and the `Hypha.Source.Extensions (LanguageSettings (..))` import.

- [ ] **Step 2: Run it, verify it fails**

Run: `cabal test all --test-options='-p Unit.Components'`
Expected: FAIL — `ciOtherModules` is not a field of `ComponentInfo`.

- [ ] **Step 3: Extend `ComponentInfo`**

Two `Language` types are in play: `Distribution.Language.Language` (what cabal parses) and `GHC.Driver.Session.Language` (what the parser wants). Translate at the boundary — do not leak cabal's type into `Hypha.Source.Extensions`:

```haskell
-- | Translate cabal's @default-language@ into the parser's language
-- selector.  Cabal admits @UnknownLanguage@ for forward compatibility;
-- an unrecognised value means "no opinion", which leaves the GHC2021
-- floor in charge.
ghcLanguageOf :: PD.Language -> Maybe GHCLang.Language
ghcLanguageOf = \case
  PD.Haskell98          -> Just GHCLang.Haskell98
  PD.Haskell2010        -> Just GHCLang.Haskell2010
  PD.GHC2021            -> Just GHCLang.GHC2021
  PD.GHC2024            -> Just GHCLang.GHC2024
  PD.UnknownLanguage _  -> Nothing
```

`GHC2024` may be absent from older `Cabal` versions; guard with CPP on `MIN_VERSION_Cabal` if the build complains, confined to this module.

```haskell
    toComponent kind lib =
      let bi   = PD.libBuildInfo lib
          raw  = map UP.getSymbolicPath (PD.hsSourceDirs bi)
          dirs = if null raw then [pkgRoot] else map (pkgRoot </>) raw
          (on, off) = splitExtensions (PD.defaultExtensions bi)
      in ComponentInfo
           { ciKind             = kind
           , ciHsSourceDirs     = dirs
           , ciExposedModules   = map renderModule (PD.exposedModules lib)
           , ciOtherModules     = map renderModule (PD.otherModules bi)
           , ciLanguageSettings = LanguageSettings
               { lsLanguage   = ghcLanguageOf =<< PD.defaultLanguage bi
               , lsDefaultOn  = on
               , lsDefaultOff = off
               }
           }

    renderModule = T.pack . render . pretty

    -- cabal models an extension as (name, enabled), and the name it
    -- carries can itself be negated (@NoImplicitPrelude@), so the two
    -- polarities compose: XNOR, not conjunction.  A cabal
    -- @DisableExtension ImplicitPrelude@ and a cabal
    -- @EnableExtension (UnknownExtension "NoImplicitPrelude")@ must
    -- reach the same answer.
    splitExtensions exts =
      let resolved =
            [ (x, cabalOn == flagOn)
            | e <- exts
            , let (nm, cabalOn) = cabalExtensionName e
            , Right pairs <- [extensionFromFlagName nm]
            , (x, flagOn) <- pairs
            ]
          unknown =
            [ u
            | e <- exts
            , let (nm, _) = cabalExtensionName e
            , Left u <- [extensionFromFlagName nm]
            ]
      in ( [ x | (x, True)  <- resolved ]
         , [ x | (x, False) <- resolved ]
         , unknown
         )

    cabalExtensionName = \case
      PD.EnableExtension  k  -> (Text.pack (show k), True)
      PD.DisableExtension k  -> (Text.pack (show k), False)
      PD.UnknownExtension nm -> (Text.pack nm, True)
```

`splitExtensions` returns the unresolved names as its third component, and `ComponentInfo` carries them:

```haskell
  , ciUnknownExtensions :: ![UnknownExtension]
    -- ^ @default-extensions@ entries GHC's flag table did not
    -- recognise.  Carried rather than dropped: an unrecognised
    -- extension is a plausible cause of a downstream parse failure, and
    -- the indexer reports these alongside the failures they explain.
```

with the construction

```haskell
           , ciLanguageSettings = LanguageSettings
               { lsLanguage   = ghcLanguageOf =<< PD.defaultLanguage bi
               , lsDefaultOn  = on
               , lsDefaultOff = off
               }
           , ciUnknownExtensions = unknown
```

and one more assertion in the Step-1 test:

```haskell
          ciUnknownExtensions c @?= []
```

- [ ] **Step 4: Run the test**

Run: `cabal test all --test-options='-p Unit.Components'`
Expected: PASS.

- [ ] **Step 5: Fix the other consumers**

`grep -rn 'ComponentInfo' src test` — every record construction needs the two new fields. `Hypha/Command/Server.hs`'s `componentsForUnit` and the mock in `test/Unit/Server.hs` are the known sites.

Run: `cabal build all && cabal test all`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add src/Hypha/Project/Components.hs test/Unit/Components.hs test/fixtures/cabal/extensions.cabal
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "feat(project): read other-modules, default-extensions and default-language"
```

---

### Task 4: `Hypha.Source.Interface` — one parse, whole module

**Files:**
- Create: `src/Hypha/Source/Interface.hs`
- Create: `test/Unit/SourceInterface.hs`
- Create: `test/fixtures/reexport/src/Fixture/Wrapper.hs`, `test/fixtures/reexport/src/Fixture/Strict.hs`, `test/fixtures/reexport/src/Fixture/Other.hs`, `test/fixtures/reexport/src/script.hs`, `test/fixtures/reexport/src/Fixture/Renamed.hs`, `test/fixtures/reexport/reexport.cabal`
- Modify: `hypha.cabal`, `test/Main.hs`

**Interfaces:**
- Consumes: `parseModuleDocWith`, `ParseError` (Task 2); `LanguageSettings` (Task 1); `ModulePath`, `SymbolName` (`Hypha.Types.SymbolPath`).
- Produces:
  ```haskell
  newtype SrcLine = SrcLine { unSrcLine :: Int }
  data ModuleInterface = ModuleInterface
    { miName      :: !ModulePath
    , miExports   :: !(Maybe [ExportItem])   -- ^ Nothing = no explicit list
    , miImports   :: ![ImportItem]
    , miDecls     :: ![Decl]
    , miHeaderDoc :: !(Maybe Text)
    }
  data ExportItem
    = ExportSymbol !SymbolName ![SymbolName]   -- ^ name + subordinates
    | ExportModule !ModulePath
  data ImportItem = ImportItem
    { iiModule  :: !ModulePath
    , iiNames   :: !(Maybe (Bool, [SymbolName]))  -- ^ (isHiding, names)
    }
  parseInterface :: LanguageSettings -> FilePath -> Text -> Either ParseError ModuleInterface
  interfaceExportedNames :: ModuleInterface -> [SymbolName]
  declaredNames :: ModuleInterface -> [SymbolName]
  ```

- [ ] **Step 1: Create the remaining fixture modules**

`test/fixtures/reexport/src/Fixture/Wrapper.hs`:

```haskell
-- | Public face of the fixture package: re-exports the internal
-- definitions the way @Data.Map.Strict@ re-exports
-- @Data.Map.Strict.Internal@.
module Fixture.Wrapper
  ( Bag (..)
  , insertBag
  , sizeBag
  , module Fixture.Other
  ) where

import Fixture.Internal (Bag (..), insertBag, sizeBag)
import Fixture.Other
```

`test/fixtures/reexport/src/Fixture/Strict.hs`:

```haskell
-- | A second wrapper over a *different* definition of the same name, so
-- the collapse rule has a case where two results must stay distinct.
module Fixture.Strict
  ( insertBag
  ) where

import Fixture.StrictInternal (insertBag)
```

`test/fixtures/reexport/src/Fixture/StrictInternal.hs`:

```haskell
-- | Strict counterpart of 'Fixture.Internal.insertBag'.  Same name,
-- same signature, different definition site.
module Fixture.StrictInternal
  ( insertBag
  ) where

import Fixture.Internal (Bag (..))

-- | Strict insert.
insertBag :: a -> Bag a -> Bag a
insertBag !x (Bag xs) = Bag (x : xs)
```

(Add `{-# LANGUAGE BangPatterns #-}` at its top — the bang is the point.)

`test/fixtures/reexport/src/Fixture/Other.hs`:

```haskell
-- | Declares a name that also exists in 'Fixture.Internal', so
-- definition-site resolution has an ambiguity to resolve.
module Fixture.Other
  ( sizeBag
  , otherOnly
  ) where

-- | Same name as 'Fixture.Internal.sizeBag', different definition.
sizeBag :: [a] -> Int
sizeBag = length

-- | Unique to this module.
otherOnly :: Bool
otherOnly = True
```

`test/fixtures/reexport/src/script.hs` (a stray file that must never become a module):

```haskell
main :: IO ()
main = putStrLn "not a library module"
```

`test/fixtures/reexport/src/Fixture/Renamed.hs` (header disagrees with the path):

```haskell
-- | Declares @Fixture.Declared@ although it lives at
-- @Fixture/Renamed.hs@.  The parse tree is the authority.
module Fixture.Declared (declaredHere) where

declaredHere :: Int
declaredHere = 1
```

`test/fixtures/reexport/reexport.cabal`:

```cabal
cabal-version: 2.4
name:          reexport
version:       0.1.0

library
  hs-source-dirs:     src
  exposed-modules:    Fixture.Wrapper
                    , Fixture.Strict
                    , Fixture.Other
                    , Fixture.Internal
  other-modules:      Fixture.StrictInternal
                    , Fixture.Declared
  default-language:   GHC2021
  build-depends:      base
```

`Fixture.Internal` is deliberately **exposed** — that is the containers
situation, where `.Internal` is public and still should lose to its
wrapper.

- [ ] **Step 2: Write the failing test**

Create `test/Unit/SourceInterface.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for 'Hypha.Source.Interface': the single parse-derived
-- view of a module that the indexer, the module page and the symbol
-- card all read.  Replaces the crude header scraping in
-- "Hypha.Source.Locate".
module Unit.SourceInterface (tests) where

import qualified Data.Text    as Text
import qualified Data.Text.IO as TIO

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Source.Extensions (defaultLanguageSettings)
import Hypha.Source.Interface
  ( ExportItem (..), ImportItem (..), ModuleInterface (..)
  , declaredNames, interfaceExportedNames, parseInterface )
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

load :: FilePath -> IO ModuleInterface
load fp = do
  src <- TIO.readFile fp
  case parseInterface defaultLanguageSettings fp src of
    Left e  -> fail ("parse failed for " <> fp <> ": " <> show e)
    Right i -> pure i

tests :: TestTree
tests = testGroup "Unit.SourceInterface"
  [ testCase "module name comes from the parse tree, not the path" $ do
      i <- load "test/fixtures/reexport/src/Fixture/Renamed.hs"
      miName i @?= ModulePath "Fixture.Declared"

  , testCase "explicit export list is recorded with subordinates" $ do
      i <- load "test/fixtures/reexport/src/Fixture/Internal.hs"
      let names = map unSymbolName (interfaceExportedNames i)
      assertBool "Bag exported"        ("Bag" `elem` names)
      assertBool "insertBag exported"  ("insertBag" `elem` names)
      assertBool "internalOnly exported" ("internalOnly" `elem` names)

  , testCase "module re-export form is preserved, not flattened away" $ do
      i <- load "test/fixtures/reexport/src/Fixture/Wrapper.hs"
      assertBool "module Fixture.Other present"
        (ExportModule (ModulePath "Fixture.Other") `elem` maybe [] id (miExports i))

  , testCase "imports carry their explicit name lists" $ do
      i <- load "test/fixtures/reexport/src/Fixture/Wrapper.hs"
      let fromInternal =
            [ ii | ii <- miImports i, iiModule ii == ModulePath "Fixture.Internal" ]
      case fromInternal of
        [ii] -> case iiNames ii of
          Just (hiding, ns) -> do
            hiding @?= False
            assertBool "insertBag listed"
              (SymbolName "insertBag" `elem` ns)
          Nothing -> fail "expected an explicit import list"
        _ -> fail "expected exactly one import of Fixture.Internal"

  , testCase "no export list means Nothing, and decls are still listed" $ do
      i <- load "test/fixtures/reexport/src/script.hs"
      miExports i @?= Nothing
      map unSymbolName (declaredNames i) @?= ["main"]

  , testCase "declared names exclude re-exports" $ do
      i <- load "test/fixtures/reexport/src/Fixture/Wrapper.hs"
      declaredNames i @?= []
  ]
```

- [ ] **Step 3: Run it, verify it fails**

Run: `cabal test all --test-options='-p Unit.SourceInterface'`
Expected: FAIL — `Could not find module 'Hypha.Source.Interface'`.

- [ ] **Step 4: Write the module**

Create `src/Hypha/Source/Interface.hs`. Key points: read `hsmodName`, `hsmodExports`, `hsmodImports` from the `HsModule GhcPs` that `parseModuleDocWith` already builds — so extend Task 2's parser to return the module rather than only `(header, decls)`, by adding one function beside it:

```haskell
-- In Hypha.Source.Parser, exported additionally:
parseModuleWith
  :: Extensions.LanguageSettings -> FilePath -> Text
  -> Either ParseError (HsModule GhcPs, Maybe Text, [Decl])
```

Then `Hypha.Source.Interface`:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | The parse-derived view of a single module: what it is called, what
-- it exports, what it imports, and what it declares.
--
-- Everything downstream reads this instead of re-deriving facts from
-- file paths or scraping the module header with a regex.  The
-- @module M ( ... ) where@ scraper in "Hypha.Source.Locate" got
-- @Type(..)@ bundles and @module N@ re-export forms wrong, and a path
-- can disagree with the module it contains — both of which cost us
-- correct search rows.
module Hypha.Source.Interface
  ( SrcLine (..)
  , ModuleInterface (..)
  , ExportItem (..)
  , ImportItem (..)
  , parseInterface
  , interfaceExportedNames
  , declaredNames
  ) where
```

Body:

```haskell
-- | A 1-based line in a source file.
newtype SrcLine = SrcLine { unSrcLine :: Int }
  deriving stock (Show, Eq, Ord)

data ModuleInterface = ModuleInterface
  { miName      :: !ModulePath
  , miExports   :: !(Maybe [ExportItem])
    -- ^ 'Nothing' when the module has no explicit export list, which
    -- means "everything declared here" — a different fact from an empty
    -- list, so it gets a different representation.
  , miImports   :: ![ImportItem]
  , miDecls     :: ![Decl]
  , miHeaderDoc :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

data ExportItem
  = ExportSymbol !SymbolName ![SymbolName]
    -- ^ A name and its listed subordinates (@Map(..)@ lists none —
    -- the definition site supplies them).
  | ExportModule !ModulePath
    -- ^ The @module M@ re-export form.  Kept rather than flattened,
    -- because flattening it needs the target's export list, which is
    -- "Hypha.Search.Reexport"'s job, not ours.
  deriving stock (Show, Eq)

data ImportItem = ImportItem
  { iiModule :: !ModulePath
  , iiNames  :: !(Maybe (Bool, [SymbolName]))
    -- ^ @Just (isHiding, names)@ for an explicit list; 'Nothing' for an
    -- unrestricted import.
  }
  deriving stock (Show, Eq)

parseInterface
  :: Extensions.LanguageSettings
  -> FilePath
  -> Text
  -> Either Parser.ParseError ModuleInterface
parseInterface ls path src = do
  (hsMod, header, decls) <- Parser.parseModuleWith ls path src
  pure ModuleInterface
    { miName      = moduleNameOf hsMod
    , miExports   = exportsOf hsMod
    , miImports   = importsOf hsMod
    , miDecls     = decls
    , miHeaderDoc = header
    }

-- | A module with no @module … where@ header is an implicit @Main@,
-- which is what GHC assumes and what the file at
-- @test/fixtures/reexport/src/script.hs@ is.
moduleNameOf :: HsModule GhcPs -> ModulePath
moduleNameOf m = case hsmodName m of
  Nothing -> ModulePath "Main"
  Just ln -> ModulePath (Text.pack (moduleNameString (unLoc ln)))

exportsOf :: HsModule GhcPs -> Maybe [ExportItem]
exportsOf m = fmap (mapMaybe (exportItem . unLoc) . unLoc) (hsmodExports m)

exportItem :: IE GhcPs -> Maybe ExportItem
exportItem = \case
  IEVar        _ n _      -> Just (ExportSymbol (wrapped n) [])
  IEThingAbs   _ n _      -> Just (ExportSymbol (wrapped n) [])
  IEThingAll   _ n _      -> Just (ExportSymbol (wrapped n) [])
  IEThingWith  _ n _ ss _ -> Just (ExportSymbol (wrapped n) (map wrapped ss))
  IEModuleContents _ lm   -> Just (ExportModule
                                    (ModulePath (Text.pack (moduleNameString (unLoc lm)))))
  -- Haddock section structure, not names.
  IEGroup{}    -> Nothing
  IEDoc{}      -> Nothing
  IEDocNamed{} -> Nothing

importsOf :: HsModule GhcPs -> [ImportItem]
importsOf m =
  [ ImportItem
      { iiModule = ModulePath (Text.pack (moduleNameString (unLoc (ideclName d))))
      , iiNames  = case ideclImportList d of
          Nothing              -> Nothing
          Just (interp, lNames) ->
            Just ( interp == EverythingBut
                 , mapMaybe (importedName . unLoc) (unLoc lNames)
                 )
      }
  | d <- map unLoc (hsmodImports m)
  ]

-- | An import list entry contributes the names it mentions, including
-- subordinates: @import M (Map(..), insertWith)@ imports both.
importedName :: IE GhcPs -> Maybe SymbolName
importedName ie = case exportItem ie of
  Just (ExportSymbol n _) -> Just n
  _                       -> Nothing

-- | Reuse "Hypha.Source.Parser"'s occurrence rendering rather than
-- writing a second one, so the index and the interface agree on what a
-- name is spelled like.
wrapped :: LIEWrappedName GhcPs -> SymbolName
wrapped = SymbolName . Parser.renderRdrName . ieWrappedName . unLoc

interfaceExportedNames :: ModuleInterface -> [SymbolName]
interfaceExportedNames i = case miExports i of
  Nothing    -> declaredNames i
  Just items -> concat [ n : subs | ExportSymbol n subs <- items ]

declaredNames :: ModuleInterface -> [SymbolName]
declaredNames i =
  [ SymbolName n
  | d <- miDecls i
  , n <- Parser.declName d : Parser.declSiblings d
  ]
```

Two supporting exports are needed from `Hypha.Source.Parser`: `parseModuleWith` (added above) and `renderRdrName :: RdrName -> Text`, which already exists there as a local helper — export it rather than duplicating it.

GHC AST constructor **arities** shift between releases even when the names do not. The bodies above are written against `ghc-lib-parser-9.10`; if 9.12 adds or drops a field, add or remove a wildcard. Do **not** work around an arity change with a catch-all `_ -> Nothing` in `exportItem` — that would silently drop export forms, which is the class of bug this module exists to remove. The `case` is deliberately total over the constructor set.

`SrcLine` lands here but `Decl`'s `Int` line fields migrate to it in Task 13, keeping this task's diff to one new module plus two parser exports.

- [ ] **Step 5: Run the test**

Run: `cabal test all --test-options='-p Unit.SourceInterface'`
Expected: PASS. `IE` constructor names shift between GHC versions; if one does not resolve, list them with:

```bash
grep -rn 'IEThingWith\|IEModuleContents' ~/.cabal/store/*/ghc-lib-parser-9.10*/lib --include='*.hi' 2>/dev/null | head
```

- [ ] **Step 6: Commit**

```bash
git add src/Hypha/Source/Interface.hs src/Hypha/Source/Parser.hs \
        test/Unit/SourceInterface.hs test/fixtures/reexport hypha.cabal test/Main.hs
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "feat(source): derive a module's interface from its parse tree"
```

---

### Task 5: `Hypha.Search.Reexport` — definition sites, not names

**Files:**
- Create: `src/Hypha/Search/Reexport.hs`
- Create: `test/Unit/SearchReexport.hs`
- Modify: `hypha.cabal`, `test/Main.hs`

**Interfaces:**
- Consumes: `ModuleInterface`, `ExportItem`, `ImportItem` (Task 4).
- Produces:
  ```haskell
  data DefinitionSite
    = DefinedHere
    | DefinedIn      !ModulePath
    | DefinedOutside !ModulePath
  data Ambiguity
    = Unambiguous
    | ResolvedAmongst !(NonEmpty ModulePath)   -- ^ the rejected candidates
  data Resolution = Resolution
    { resSite      :: !DefinitionSite
    , resAmbiguity :: !Ambiguity
    }
  resolveComponent :: [ModuleInterface] -> Map (ModulePath, SymbolName) Resolution
  definitionModule :: ModulePath -> DefinitionSite -> ModulePath
  ```
  `definitionModule` folds `DefinedHere` back to the asking module so
  callers never case-split just to get a `ModulePath`.

- [ ] **Step 1: Write the failing test**

Create `test/Unit/SearchReexport.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for 'Hypha.Search.Reexport'.
--
-- The bug this module exists to kill: resolving a re-export by symbol
-- *name* gave @Data.IntMap.Lazy.insertWith@ the signature of
-- @Data.Map.insertWith@, because a name-keyed map cannot tell two
-- same-named definitions apart.  Every case below pins a shape where
-- name-keyed resolution would be wrong.
module Unit.SearchReexport (tests) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map
import qualified Data.Text.IO       as TIO

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Source.Extensions (defaultLanguageSettings)
import Hypha.Source.Interface  (ModuleInterface, parseInterface)
import Hypha.Search.Reexport
  ( Ambiguity (..), DefinitionSite (..), Resolution (..), resolveComponent )
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

component :: IO [ModuleInterface]
component = mapM load
  [ "test/fixtures/reexport/src/Fixture/Internal.hs"
  , "test/fixtures/reexport/src/Fixture/Wrapper.hs"
  , "test/fixtures/reexport/src/Fixture/Strict.hs"
  , "test/fixtures/reexport/src/Fixture/StrictInternal.hs"
  , "test/fixtures/reexport/src/Fixture/Other.hs"
  ]
  where
    load fp = do
      src <- TIO.readFile fp
      case parseInterface defaultLanguageSettings fp src of
        Left e  -> fail ("parse failed for " <> fp <> ": " <> show e)
        Right i -> pure i

siteOf :: Map.Map (ModulePath, SymbolName) Resolution -> (ModulePath, SymbolName) -> IO DefinitionSite
siteOf m k = maybe (fail ("no resolution for " <> show k)) (pure . resSite) (Map.lookup k m)

tests :: TestTree
tests = testGroup "Unit.SearchReexport"
  [ testCase "a locally declared name resolves to itself" $ do
      r <- resolveComponent <$> component
      s <- siteOf r (ModulePath "Fixture.Internal", SymbolName "insertBag")
      s @?= DefinedHere

  , testCase "a re-export resolves to the module that declares it" $ do
      r <- resolveComponent <$> component
      s <- siteOf r (ModulePath "Fixture.Wrapper", SymbolName "insertBag")
      s @?= DefinedIn (ModulePath "Fixture.Internal")

  , testCase "same name, different wrappers, different definitions" $ do
      -- The case a name-keyed map gets wrong: Fixture.Strict.insertBag
      -- and Fixture.Wrapper.insertBag share a name and a signature and
      -- must NOT share a definition site.
      r  <- resolveComponent <$> component
      s1 <- siteOf r (ModulePath "Fixture.Wrapper", SymbolName "insertBag")
      s2 <- siteOf r (ModulePath "Fixture.Strict",  SymbolName "insertBag")
      s1 @?= DefinedIn (ModulePath "Fixture.Internal")
      s2 @?= DefinedIn (ModulePath "Fixture.StrictInternal")

  , testCase "module re-export form contributes the target's exports" $ do
      r <- resolveComponent <$> component
      s <- siteOf r (ModulePath "Fixture.Wrapper", SymbolName "otherOnly")
      s @?= DefinedIn (ModulePath "Fixture.Other")

  , testCase "ambiguity is resolved by import, and recorded" $ do
      -- Fixture.Wrapper re-exports sizeBag explicitly (from
      -- Fixture.Internal) while also re-exporting all of Fixture.Other,
      -- which declares its own sizeBag.  The explicit import decides
      -- it; the rejected candidate is kept.
      r <- resolveComponent <$> component
      case Map.lookup (ModulePath "Fixture.Wrapper", SymbolName "sizeBag") r of
        Nothing  -> fail "no resolution for Fixture.Wrapper.sizeBag"
        Just res -> do
          resSite res @?= DefinedIn (ModulePath "Fixture.Internal")
          case resAmbiguity res of
            ResolvedAmongst rejected ->
              NE.toList rejected @?= [ModulePath "Fixture.Other"]
            Unambiguous -> fail "expected the rejected candidate to be recorded"

  , testCase "a name no component module declares is DefinedOutside" $ do
      r <- resolveComponent <$> component
      -- Fixture.Internal exports Bag(..); its constructor comes from a
      -- local decl, but `length` (imported from base and not re-exported)
      -- must not be invented.  Assert the negative: no row claims a
      -- definition we do not have.
      Map.lookup (ModulePath "Fixture.Internal", SymbolName "length") r @?= Nothing

  , testCase "a mutual re-export cycle terminates instead of diverging" $ do
      -- A re-exports x from B while B re-exports x from A.  Neither
      -- declares it, so neither can resolve — but the resolver must
      -- notice the cycle rather than recurse forever.  A test that
      -- hangs here is a failing test.
      let mkIface m imp = ModuleInterface
            { miName      = ModulePath m
            , miExports   = Just [ExportSymbol (SymbolName "x") []]
            , miImports   = [ImportItem (ModulePath imp) Nothing]
            , miDecls     = []
            , miHeaderDoc = Nothing
            }
          r = resolveComponent [mkIface "A" "B", mkIface "B" "A"]
      resSite <$> Map.lookup (ModulePath "A", SymbolName "x") r
        @?= Just (DefinedOutside (ModulePath "B"))
      resSite <$> Map.lookup (ModulePath "B", SymbolName "x") r
        @?= Just (DefinedOutside (ModulePath "A"))
  ]
```

Add `ExportItem (..)`, `ImportItem (..)` and `ModuleInterface (..)` to this file's `Hypha.Source.Interface` import list — the cycle case builds interfaces directly rather than through a fixture, because no legal on-disk fixture produces this shape without also declaring the name somewhere.

- [ ] **Step 2: Run it, verify it fails**

Run: `cabal test all --test-options='-p Unit.SearchReexport'`
Expected: FAIL — `Could not find module 'Hypha.Search.Reexport'`.

- [ ] **Step 3: Write the module**

Create `src/Hypha/Search/Reexport.hs`:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Resolve each module's exported names to the module that actually
-- declares them.
--
-- The predecessor of this module was a @Map SymbolName Signature@ built
-- from every local declaration in the component.  That map cannot
-- distinguish two definitions of one name, so
-- @Data.IntMap.Lazy.insertWith@ was published with
-- @Data.Map.insertWith@'s signature.  Resolution has to be per
-- @(module, name)@ pair and has to follow imports, or it is guessing.
module Hypha.Search.Reexport
  ( DefinitionSite (..)
  , Ambiguity (..)
  , Resolution (..)
  , resolveComponent
  , definitionModule
  , sharedSegments
  ) where

data DefinitionSite
  = DefinedHere
  | DefinedIn !ModulePath
    -- ^ Declared by another module of the same component.
  | DefinedOutside !ModulePath
    -- ^ No module of this component declares it; the field names the
    -- import we believe supplies it.  No row is indexed for these — we
    -- have no signature, and the definition belongs to another
    -- component's index entry.
  deriving stock (Show, Eq)

data Ambiguity
  = Unambiguous
  | ResolvedAmongst !(NonEmpty ModulePath)
    -- ^ The candidates we rejected, kept so the choice is testable and
    -- reportable instead of an accident of list order.
  deriving stock (Show, Eq)

data Resolution = Resolution
  { resSite      :: !DefinitionSite
  , resAmbiguity :: !Ambiguity
  }
  deriving stock (Show, Eq)

-- | Fold 'DefinedHere' back to the asking module so callers that only
-- want a 'ModulePath' need not case-split.
definitionModule :: ModulePath -> DefinitionSite -> ModulePath
definitionModule asking = \case
  DefinedHere      -> asking
  DefinedIn m      -> m
  DefinedOutside m -> m

resolveComponent :: [ModuleInterface] -> Map (ModulePath, SymbolName) Resolution
resolveComponent ifaces = Map.fromList
  [ ((miName i, n), resolve Set.empty i n)
  | i <- ifaces
  , n <- Interface.interfaceExportedNames i ++ moduleFormNames i
  ]
  where
    byName = Map.fromList [ (miName i, i) | i <- ifaces ]

    declares i n = n `elem` Interface.declaredNames i

    -- Names contributed by @module M@ export forms: the target's own
    -- exported names, one level at a time (the recursion below walks
    -- deeper chains).
    moduleFormNames i =
      [ n
      | Just items <- [miExports i]
      , ExportModule m <- items
      , Just target <- [Map.lookup m byName]
      , n <- Interface.interfaceExportedNames target
      ]

    -- @visiting@ is the set of modules already on the resolution stack.
    -- A candidate that is on the stack is dropped, so a mutual
    -- re-export terminates with DefinedOutside instead of recursing.
    resolve visiting i n
      | declares i n = Resolution DefinedHere Unambiguous
      | otherwise =
          let visiting' = Set.insert (miName i) visiting
              (preferred, open) = candidates i n
              viable ms =
                [ m
                | m <- ms
                , not (m `Set.member` visiting')
                , Just target <- [Map.lookup m byName]
                , suppliesName visiting' target n
                ]
              ranked = case viable preferred of
                []  -> rank i (viable open)
                ps  -> rank i ps
          in case ranked of
               (winner : rejected) -> Resolution
                 (DefinedIn winner)
                 (maybe Unambiguous ResolvedAmongst (NE.nonEmpty rejected))
               [] -> Resolution
                 (outsideFor i n)
                 Unambiguous

    -- A candidate module supplies the name if it declares it or can
    -- itself resolve it inside the component.
    suppliesName visiting target n =
      declares target n
        || case resolve visiting target n of
             Resolution (DefinedIn _) _ -> True
             _                          -> False

    -- Explicit-import candidates outrank open ones: @import M (foo)@ is
    -- a statement about where @foo@ comes from, an unrestricted import
    -- is not.
    candidates i n =
      ( [ iiModule ii | ii <- miImports i, explicitlyLists ii n ]
      , [ iiModule ii | ii <- miImports i, openImport ii n ]
          ++ [ m | Just items <- [miExports i], ExportModule m <- items ]
      )

    explicitlyLists ii n = case iiNames ii of
      Just (False, ns) -> n `elem` ns
      _                -> False

    openImport ii n = case iiNames ii of
      Nothing         -> True
      Just (True, ns) -> n `notElem` ns   -- hiding list that does not hide it
      Just (False, _) -> False

    -- Sibling modules first, then lexicographic, so the winner does not
    -- depend on declaration order.
    rank i =
      sortOn (\m -> (negate (sharedSegments (miName i) m), unModulePath m))

    -- Nothing inside the component supplies it: name the first import
    -- that plausibly does, so the module page can still list the symbol
    -- and say where it came from.
    outsideFor i n = case [ iiModule ii | ii <- miImports i
                          , explicitlyLists ii n || openImport ii n ] of
      (m : _) -> DefinedOutside m
      []      -> DefinedOutside (miName i)

-- | How many leading dot-separated segments two module paths share.
-- @Data.Map.Strict@ and @Data.Map.Internal@ share two; @Data.Map.Strict@
-- and @Data.Set.Internal@ share one.  Comparing characters instead —
-- which is what the file-path ranking in "Hypha.Source.Locate" did —
-- made those two look nearly identical.
sharedSegments :: ModulePath -> ModulePath -> Int
sharedSegments a b =
  length (takeWhile id (zipWith (==) (segments a) (segments b)))
  where segments = Text.splitOn "." . unModulePath
```

Two notes for the implementer:

- `outsideFor` returning `DefinedOutside (miName i)` when there is no
  plausible import at all keeps the function total without an `error`.
  Task 9 does not index `DefinedOutside` rows, so this value never
  reaches a row; the Step-1 test asserting `Map.lookup … @?= Nothing`
  for `length` passes because `length` is not in
  `interfaceExportedNames` at all, not because of this branch.
- `suppliesName` re-enters `resolve`, so deep chains cost repeated work.
  Measure before optimising: a component's module count is in the tens
  for the packages that matter. If a real package makes this hot, add a
  memo table threaded as `State` — do not add one speculatively.

Segment comparison helper — this is what replaces `Locate.sortByPrefix`'s dotted-versus-slashed character count:

```haskell
-- | How many leading dot-separated segments two module paths share.
-- @Data.Map.Strict@ and @Data.Map.Internal@ share two; @Data.Map.Strict@
-- and @Data.Set.Internal@ share one.  Comparing characters instead —
-- which is what the old file-path ranking did — made those look nearly
-- identical.
sharedSegments :: ModulePath -> ModulePath -> Int
sharedSegments a b =
  length (takeWhile id (zipWith (==) (segments a) (segments b)))
  where segments = Text.splitOn "." . unModulePath
```

- [ ] **Step 4: Run the test**

Run: `cabal test all --test-options='-p Unit.SearchReexport'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/Hypha/Search/Reexport.hs test/Unit/SearchReexport.hs hypha.cabal test/Main.hs
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "feat(search): resolve re-exports to a definition site"
```

---

### Task 6: `Signature` and `ComponentKey` newtypes

**Files:**
- Modify: `src/Hypha/Types/SymbolPath.hs`, `src/Hypha/Types/ComponentName.hs`
- Modify: `test/Property/SymbolPath.hs`

**Interfaces:**
- Produces:
  ```haskell
  newtype Signature = Signature { unSignature :: Text }     -- SymbolPath
  newtype ComponentKey = ComponentKey { unComponentKey :: Text }  -- ComponentName
  componentKeyOf :: PackageName -> ComponentKind -> ComponentKey
  ```
  `componentKeyOf` replaces `Command/Server.hs:478`'s `componentKey`, so
  the `pkg` / `pkg:sub` / `pkg:exe:name` encoding lives with the type it
  encodes.

- [ ] **Step 1: Write the failing test**

Append to `test/Unit/Components.hs` (`Data.Foldable (for_)` and the `Hypha.Types.ComponentName` / `Hypha.Types.PackageId` imports come with it):

```haskell
  , testCase "component keys round-trip through their rendering" $
      for_ [ (PackageName "containers", MainLib)
           , (PackageName "hypha",      SubLib "hypha-internal")
           , (PackageName "hypha",      Exe "hypha-mcp")
           ] $ \(pkg, kind) -> do
        let key = componentKeyOf pkg kind
        parseComponentKey (unComponentKey key) @?= Just (unPackageName pkg, kind)

  , testCase "a sublib name containing a colon is rejected, not mis-parsed" $
      -- The encoding is @pkg@ / @pkg:sub@ / @pkg:exe:name@, so a colon
      -- inside a component name would make it ambiguous.  Cabal forbids
      -- it; assert we do not silently accept one either.
      parseComponentKey "hypha:a:b:c" @?= Nothing
```

- [ ] **Step 2: Run it, verify it fails**

Run: `cabal test all --test-options='-p SymbolPath'`
Expected: FAIL — `componentKeyOf` not in scope.

- [ ] **Step 3: Add the newtypes**

In `src/Hypha/Types/SymbolPath.hs`:

```haskell
-- | A rendered Haskell type signature (@insertWith :: Ord k => …@).
-- The index stores these, the UI renders them, and nothing in between
-- should treat one as arbitrary text.
newtype Signature = Signature { unSignature :: Text }
  deriving stock (Show, Eq, Ord)
```

In `src/Hypha/Types/ComponentName.hs`, add `ComponentKey`, `componentKeyOf` (moved verbatim from `Command/Server.hs:478-481`, now returning `ComponentKey`) and `parseComponentKey :: Text -> Maybe (Text, ComponentKind)` as the inverse. `Command/Server.hs`'s local `componentKey` is deleted and its call sites re-pointed.

- [ ] **Step 4: Run the test suite**

Run: `cabal build all && cabal test all`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/Hypha/Types/SymbolPath.hs src/Hypha/Types/ComponentName.hs \
        src/Hypha/Command/Server.hs test/Property/SymbolPath.hs
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "refactor(types): newtype Signature and ComponentKey"
```

---

### Task 7: Cache — `IndexRow`, new columns, one-time wipe

**Files:**
- Modify: `src/Hypha/Search/Cache.hs:70-89` (schema), `:117-131` (`lookupRowsByName`), `:162-199` (`readIndex`/`writeIndex`), `:58-68` (`openIndexCache`)
- Modify: `src/Hypha/Search/PackageCache.hs` (4-tuples → `IndexRow`)
- Create: `test/Unit/SearchIndexCache.hs`
- Modify: `hypha.cabal`, `test/Main.hs`

**Interfaces:**
- Consumes: `Signature`, `ComponentKey` (Task 6); `ModulePath`, `SymbolName`.
- Produces (in `Hypha.Search.Index`, created here so the row type has a home):
  ```haskell
  data Visibility = Exposed | Internal
  data IndexRow = IndexRow
    { rowComponent  :: !ComponentKey
    , rowModule     :: !ModulePath
    , rowName       :: !SymbolName
    , rowSignature  :: !Signature
    , rowDefModule  :: !ModulePath
    , rowVisibility :: !Visibility
    }
  currentIndexFormat :: Int
  ```
  and in `Hypha.Search.Cache`:
  ```haskell
  readIndex  :: IndexCache -> Text -> Text -> IO [IndexRow]
  writeIndex :: IndexCache -> Text -> Text -> [IndexRow] -> IO ()
  lookupRowsByName :: IndexCache -> Text -> Maybe Text -> IO [IndexRow]
  ```

- [ ] **Step 1: Write the failing test**

Create `test/Unit/SearchIndexCache.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for the index cache's row shape and its format guard.
--
-- Rows written by older hypha builds carry module names derived from
-- file paths and signatures resolved by symbol name — both wrong.  The
-- format guard is what stops those from outliving the fix.
module Unit.SearchIndexCache (tests) where

import qualified Database.SQLite.Simple as Sql
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.Cache
  ( openIndexCache, readIndex, writeIndex )
import Hypha.Search.Index
  ( IndexRow (..), Visibility (..) )
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))

row :: IndexRow
row = IndexRow
  { rowComponent  = ComponentKey "containers"
  , rowModule     = ModulePath "Data.Map.Strict"
  , rowName       = SymbolName "insertWith"
  , rowSignature  = Signature "insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a"
  , rowDefModule  = ModulePath "Data.Map.Strict.Internal"
  , rowVisibility = Exposed
  }

tests :: TestTree
tests = testGroup "Unit.SearchIndexCache"
  [ testCase "rows round-trip with their definition module and visibility" $
      withSystemTempDirectory "hypha-cache" $ \dir -> do
        c    <- openIndexCache (dir </> "test.db")
        writeIndex c "containers" "0.7" [row]
        rows <- readIndex c "containers" "0.7"
        rows @?= [row]

  , testCase "a pre-format-guard database is wiped on open" $
      withSystemTempDirectory "hypha-cache" $ \dir -> do
        let path = dir </> "old.db"
        -- Simulate an old cache: correct table shape for the columns it
        -- had, no index_format key.
        conn <- Sql.open path
        Sql.execute_ conn
          "CREATE TABLE pkg_index (pkg TEXT NOT NULL, version TEXT NOT NULL, \
          \mod TEXT NOT NULL, name TEXT NOT NULL, sig TEXT NOT NULL)"
        Sql.execute_ conn
          "CREATE TABLE pkg_index_meta (pkg TEXT NOT NULL, version TEXT NOT NULL, \
          \indexed_at INTEGER NOT NULL, fingerprint TEXT, PRIMARY KEY (pkg, version))"
        Sql.execute_ conn
          "INSERT INTO pkg_index VALUES ('containers','0.7','data.map.strict','insertWith','')"
        Sql.execute_ conn
          "INSERT INTO pkg_index_meta VALUES ('containers','0.7',0,NULL)"
        Sql.close conn

        c    <- openIndexCache path
        rows <- readIndex c "containers" "0.7"
        rows @?= []

  , testCase "opening twice does not wipe rows the second time" $
      withSystemTempDirectory "hypha-cache" $ \dir -> do
        let path = dir </> "twice.db"
        c1 <- openIndexCache path
        writeIndex c1 "containers" "0.7" [row]
        c2 <- openIndexCache path
        rows <- readIndex c2 "containers" "0.7"
        rows @?= [row]
  ]
```

`withSystemTempDirectory` comes from `temporary`; check `hypha.cabal`'s test-suite `build-depends` and add it if absent (`grep -n temporary hypha.cabal`) — other cache tests (`test/Unit/PackageCache.hs`) already need temp dirs, so copy whatever they use.

- [ ] **Step 2: Run it, verify it fails**

Run: `cabal test all --test-options='-p Unit.SearchIndexCache'`
Expected: FAIL — `Hypha.Search.Index` does not exist.

- [ ] **Step 3: Create `Hypha.Search.Index` with the row type**

Create `src/Hypha/Search/Index.hs` holding, for now, only:

```haskell
-- | Whether the module presenting a symbol is part of the component's
-- public surface (@exposed-modules@) or an implementation detail
-- (@other-modules@).  Search ranks Exposed above Internal, so an
-- @.Internal@ definition never outranks the wrapper that documents it.
data Visibility = Exposed | Internal
  deriving stock (Show, Eq, Ord)

-- | One search-index entry.
--
-- 'rowDefModule' is the module that actually declares the symbol; it
-- equals 'rowModule' for a local declaration and names the definition
-- site for a re-export.  Carrying it is what lets search collapse
-- @Data.Map.Strict.Internal.insertWith@ into
-- @Data.Map.Strict.insertWith@ without merging
-- @Data.Map.Lazy.insertWith@, which has the same name and the same
-- signature but a different definition.
data IndexRow = IndexRow { … }

-- | Bumped whenever a row's meaning changes.  'openIndexCache' wipes
-- rows written under an older format rather than trusting them: every
-- pre-2 row may carry a path-derived module name or a signature
-- resolved by symbol name.
currentIndexFormat :: Int
currentIndexFormat = 2
```

- [ ] **Step 4: Extend the schema and add the format guard**

In `src/Hypha/Search/Cache.hs`:

```haskell
schema =
  [ -- unchanged
    "CREATE TABLE IF NOT EXISTS pkg_index_meta \
    \  ( pkg     TEXT NOT NULL \
    \  , version TEXT NOT NULL \
    \  , indexed_at INTEGER NOT NULL \
    \  , fingerprint TEXT \
    \  , PRIMARY KEY (pkg, version) )"
    -- two new columns
  , "CREATE TABLE IF NOT EXISTS pkg_index \
    \  ( pkg     TEXT NOT NULL \
    \  , version TEXT NOT NULL \
    \  , mod     TEXT NOT NULL \
    \  , name    TEXT NOT NULL \
    \  , sig     TEXT NOT NULL \
    \  , def_mod TEXT NOT NULL \
    \  , visibility TEXT NOT NULL )"
    -- unchanged
  , "CREATE INDEX IF NOT EXISTS pkg_index_by_pv \
    \  ON pkg_index (pkg, version)"
  , "CREATE TABLE IF NOT EXISTS kv \
    \  ( k TEXT PRIMARY KEY NOT NULL \
    \  , v BLOB NOT NULL )"
  ]

openIndexCache path = do
  conn <- open path
  execute_ conn "PRAGMA journal_mode = WAL"
  execute_ conn "PRAGMA synchronous = NORMAL"
  mapM_ (execute_ conn) schema
  migrateAddColumn conn "pkg_index_meta" "fingerprint" "TEXT"
  migrateAddColumn conn "pkg_index" "def_mod"    "TEXT NOT NULL DEFAULT ''"
  migrateAddColumn conn "pkg_index" "visibility" "TEXT NOT NULL DEFAULT ''"
  lock <- newMVar ()
  let c = IndexCache conn lock
  ensureIndexFormat c
  pure c

-- | Discard rows written under an older row format.  We do not attempt
-- to migrate them: the defects that motivated the bump (module names
-- taken from file paths, signatures resolved by symbol name) are not
-- detectable per row, so the only honest options are re-index or lie.
ensureIndexFormat :: IndexCache -> IO ()
ensureIndexFormat c = do
  stored <- readBlob c indexFormatKey
  let current = Text.pack (show Index.currentIndexFormat)
  if stored == Just current
    then pure ()
    else do
      withWrite c $ Sql.withTransaction (icConn c) $ do
        execute_ (icConn c) "DELETE FROM pkg_index"
        execute_ (icConn c) "DELETE FROM pkg_index_meta"
      writeBlob c indexFormatKey current

indexFormatKey :: Text
indexFormatKey = "index_format"
```

`Hypha.Search.Cache` importing `Hypha.Search.Index` for `currentIndexFormat` and `IndexRow` puts the row type below the SQL layer in the dependency order; keep `Index` free of any `sqlite-simple` import so the direction stays one-way.

`readIndex`/`writeIndex`/`lookupRowsByName` swap tuples for `IndexRow`, with `FromRow`/`ToRow` instances written explicitly in `Cache.hs` (not derived — the column order is a wire format and should be visible):

```haskell
instance Sql.FromRow IndexRow where
  fromRow = IndexRow
    <$> (ComponentKey <$> Sql.field)
    <*> (ModulePath   <$> Sql.field)
    <*> (SymbolName   <$> Sql.field)
    <*> (Signature    <$> Sql.field)
    <*> (ModulePath   <$> Sql.field)
    <*> (visibilityFromText =<< Sql.field)
```

`visibilityFromText` is total: an unrecognised string means an unmigrated row, and since `ensureIndexFormat` has already wiped those, treat it as `Internal` **and** trace once to stderr — do not silently default.

- [ ] **Step 5: Update `PackageCache`**

`readCachedIndex`, `lookupByName`, `writeCachedIndex` and `mergeShadow` take `[IndexRow]`. `mergeShadow`'s key becomes `(rowComponent, rowModule, rowName)`.

- [ ] **Step 6: Run the tests**

Run: `cabal build all && cabal test all`
Expected: PASS. Callers of the 4-tuple API (`Command/Server.hs`, `Command/Lookup.hs`, `Hoogle/Local.hs`, `test/Unit/PackageCache*.hs`) need mechanical updating; `grep -rn 'readCachedIndex\|lookupByName\|writeCachedIndex' src test` finds them all.

- [ ] **Step 7: Commit**

```bash
git add src/Hypha/Search/Index.hs src/Hypha/Search/Cache.hs src/Hypha/Search/PackageCache.hs \
        test/Unit/SearchIndexCache.hs hypha.cabal test/Main.hs
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "feat(search): typed index rows with definition module and visibility"
```

---

### Task 8: Move index building into `Hypha.Search.Index`, mechanically

**Files:**
- Modify: `src/Hypha/Command/Server.hs` (remove `buildAndCacheIndex`, `collectModuleRows`, `reexportRows`, `hydrateFromCache`, `enumModulesIn`, `chooseSourceRoots`, `componentsForUnit`, `findHs`, `hsToModule`)
- Modify: `src/Hypha/Search/Index.hs` (receive them verbatim)
- Modify: `test/Unit/Server.hs` (import from the new home)

**Interfaces:**
- Consumes: everything Task 7 produced.
- Produces: the moved functions, same names, same behaviour:
  ```haskell
  buildAndCacheIndex :: BuildPlan -> HyphaPackageCache -> PackageResolver IO
                     -> [PackageId] -> IORef [Fuzzy.IndexedRow] -> IORef Int -> IO ()
  hydrateFromCache   :: BuildPlan -> HyphaPackageCache -> [PackageId]
                     -> IORef [Fuzzy.IndexedRow] -> IO [PackageId]
  componentsForUnit  :: BuildPlan -> PackageId -> FilePath -> IO [(ComponentKind, [FilePath])]
  enumModulesIn      :: [FilePath] -> IO [Text]
  chooseSourceRoots  :: FilePath -> IO [FilePath]
  ```

This task changes **no behaviour**. It exists so Task 9's diff is readable: a 817-line module that owns both the server wiring and the indexer cannot be reviewed for a semantic change at the same time as the move.

- [ ] **Step 1: Move the functions**

Cut the listed functions from `Command/Server.hs` into `Search/Index.hs`, keeping their Haddock verbatim. Export them from `Search.Index`; import them qualified in `Command/Server.hs`.

- [ ] **Step 2: Confirm the golden output is byte-identical**

Run: `cabal build all && cabal test all`
Expected: PASS with no golden-file changes. `git diff --stat test/Golden/golden` must be empty. If a golden file moved, the "mechanical" move was not mechanical — find the behaviour change before continuing.

- [ ] **Step 3: Commit**

```bash
git add src/Hypha/Command/Server.hs src/Hypha/Search/Index.hs test/Unit/Server.hs
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "refactor(search): move index building out of Command.Server"
```

---

### Task 9: Build rows from cabal modules, parse-tree names and definition sites

**Files:**
- Modify: `src/Hypha/Search/Index.hs`
- Create: `test/Unit/SearchIndexBuild.hs`
- Modify: `hypha.cabal`, `test/Main.hs`

**Interfaces:**
- Consumes: `ComponentInfo` with `ciOtherModules`/`ciLanguageSettings` (Task 3); `parseInterface` (Task 4); `resolveComponent` (Task 5); `IndexRow` (Task 7).
- Produces:
  ```haskell
  data ModuleSource = ModuleSource
    { msDeclaredName :: !ModulePath      -- ^ from the cabal stanza / path walk
    , msPath         :: !FilePath
    , msVisibility   :: !Visibility
    , msContent      :: !Text
    }
  data ComponentIndex = ComponentIndex
    { ciRows          :: ![IndexRow]
    , ciParseFailures :: ![(ModulePath, ParseError)]
    , ciNameMismatch  :: ![(ModulePath, ModulePath)]  -- ^ (declared, actual)
    }
  indexComponentPure :: ComponentKey -> LanguageSettings -> [ModuleSource] -> ComponentIndex
  ```
  `indexComponentPure` is where the correctness lives, and it is pure —
  the IO shell only reads files and reports.

- [ ] **Step 1: Create the shared fixture loader**

Tasks 9, 12 and 14 all need the fixture component as `[ModuleSource]`. One definition, in `test/Util/Fixture.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- | Loader for the @test/fixtures/reexport@ component, shared by the
-- index, locate and module-doc suites so they cannot drift apart on
-- what the fixture contains.
module Util.Fixture
  ( fixtureSources
  , sourcesFor
  ) where

import           Data.Text (Text)
import qualified Data.Text.IO as TIO

import Hypha.Search.Index (ModuleSource (..), Visibility (..))
import Hypha.Types.SymbolPath (ModulePath (..))

sourcesFor :: [(FilePath, Text, Visibility)] -> IO [ModuleSource]
sourcesFor = mapM $ \(fp, declared, vis) -> do
  content <- TIO.readFile fp
  pure ModuleSource
    { msDeclaredName = ModulePath declared
    , msPath         = fp
    , msVisibility   = vis
    , msContent      = content
    }

-- | The fixture component, with the visibility its cabal file declares.
-- @Fixture.Internal@ is deliberately exposed: that is the @containers@
-- situation, where an @.Internal@ module is public and must still lose
-- to the wrapper that documents it.
fixtureSources :: IO [ModuleSource]
fixtureSources = sourcesFor
  [ ("test/fixtures/reexport/src/Fixture/Internal.hs",       "Fixture.Internal",       Exposed)
  , ("test/fixtures/reexport/src/Fixture/Wrapper.hs",        "Fixture.Wrapper",        Exposed)
  , ("test/fixtures/reexport/src/Fixture/Strict.hs",         "Fixture.Strict",         Exposed)
  , ("test/fixtures/reexport/src/Fixture/StrictInternal.hs", "Fixture.StrictInternal", Internal)
  , ("test/fixtures/reexport/src/Fixture/Other.hs",          "Fixture.Other",          Exposed)
  , ("test/fixtures/reexport/src/Fixture/Renamed.hs",        "Fixture.Declared",       Internal)
  ]
```

Add `Util.Fixture` to the test-suite's `other-modules`.

- [ ] **Step 2: Write the failing test**

Create `test/Unit/SearchIndexBuild.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for row construction: which modules exist, what they are
-- called, and where each symbol is defined.
--
-- Every case corresponds to a row shape observed in a real cache and
-- known to be wrong: @data.map.strict.internal@ (lowercase, from a file
-- path), @compiler.GHC.Data.Word64Map.Internal@ (source-dir segment in
-- the name), @concasync@ (a script mistaken for a module), and
-- @Data.IntMap.Lazy.insertWith :: … Map k a@ (signature resolved by
-- name).
module Unit.SearchIndexBuild (tests) where

import           Data.List (sort)
import qualified Data.Text    as Text
import qualified Data.Text.IO as TIO

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Search.Index
  ( ComponentIndex (..), IndexRow (..), ModuleSource (..), Visibility (..)
  , indexComponentPure )
import Hypha.Source.Extensions (defaultLanguageSettings)
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))
import Util.Fixture (fixtureSources, sourcesFor)

fixture :: IO ComponentIndex
fixture = do
  srcs <- fixtureSources
  pure (indexComponentPure (ComponentKey "reexport") defaultLanguageSettings srcs)

rowsFor :: ComponentIndex -> Text -> [IndexRow]
rowsFor ci n = [ r | r <- ciRows ci, rowName r == SymbolName n ]

tests :: TestTree
tests = testGroup "Unit.SearchIndexBuild"
  [ testCase "every module name is the one the source declares" $ do
      ci <- fixture
      let mods = sort (map (unModulePath . rowModule) (ciRows ci))
      assertBool "no lowercase-only module name"
        (all (\m -> m /= Text.toLower m) mods)
      assertBool "Fixture.Declared present (header beats path)"
        (ModulePath "Fixture.Declared" `elem` map rowModule (ciRows ci))

  , testCase "a path/header disagreement is reported" $ do
      ci <- fixture
      ciNameMismatch ci @?= []   -- declared name already matches here
      -- Feed a deliberate mismatch and assert it is reported:
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/Renamed.hs", "Fixture.Renamed", Internal) ]
      let ci' = indexComponentPure (ComponentKey "reexport") defaultLanguageSettings srcs
      ciNameMismatch ci' @?= [(ModulePath "Fixture.Renamed", ModulePath "Fixture.Declared")]
      assertBool "rows use the declared-in-source name"
        (all ((== ModulePath "Fixture.Declared") . rowModule) (ciRows ci'))

  , testCase "a re-exported symbol gets a row on the wrapper" $ do
      ci <- fixture
      let mods = sort (map (unModulePath . rowModule) (rowsFor ci "insertBag"))
      mods @?= ["Fixture.Internal", "Fixture.Strict", "Fixture.StrictInternal", "Fixture.Wrapper"]

  , testCase "each row's definition module is the real one" $ do
      ci <- fixture
      let defOf m = [ rowDefModule r | r <- rowsFor ci "insertBag", rowModule r == ModulePath m ]
      defOf "Fixture.Wrapper"        @?= [ModulePath "Fixture.Internal"]
      defOf "Fixture.Strict"         @?= [ModulePath "Fixture.StrictInternal"]
      defOf "Fixture.Internal"       @?= [ModulePath "Fixture.Internal"]
      defOf "Fixture.StrictInternal" @?= [ModulePath "Fixture.StrictInternal"]

  , testCase "signatures come from the definition site, never by name" $ do
      -- The Data.IntMap.Lazy regression: two definitions share a name,
      -- and the wrapper must not inherit the other one's signature.
      ci <- fixture
      let sigOf m = [ unSignature (rowSignature r)
                    | r <- rowsFor ci "sizeBag", rowModule r == ModulePath m ]
      sigOf "Fixture.Other" @?= ["sizeBag :: [a] -> Int"]
      assertBool "wrapper takes Fixture.Internal's Bag signature"
        (all (Text.isInfixOf "Bag a -> Int") (sigOf "Fixture.Wrapper"))

  , testCase "visibility follows the cabal stanza" $ do
      ci <- fixture
      let visOf m = [ rowVisibility r | r <- ciRows ci, rowModule r == ModulePath m ]
      assertBool "StrictInternal rows are Internal"
        (all (== Internal) (visOf "Fixture.StrictInternal"))
      assertBool "Wrapper rows are Exposed"
        (all (== Exposed) (visOf "Fixture.Wrapper"))

  , testCase "a module that cannot be parsed yields no invented rows" $ do
      let broken = ModuleSource
            { msDeclaredName = ModulePath "Fixture.Broken"
            , msPath         = "Fixture/Broken.hs"
            , msVisibility   = Exposed
            , msContent      = "module Fixture.Broken (f) where\nf x = case x of\n  -> 1\n"
            }
          ci = indexComponentPure (ComponentKey "reexport") defaultLanguageSettings [broken]
      ciRows ci @?= []
      assertBool "the failure is reported" (length (ciParseFailures ci) == 1)
  ]
```

- [ ] **Step 3: Run it, verify it fails**

Run: `cabal test all --test-options='-p Unit.SearchIndexBuild'`
Expected: FAIL — `indexComponentPure` not in scope.

- [ ] **Step 4: Write `indexComponentPure`**

In `src/Hypha/Search/Index.hs`:

```haskell
-- | One module's bytes plus the two facts only the cabal stanza knows:
-- which name the stanza expected, and whether the stanza exposes it.
data ModuleSource = ModuleSource
  { msDeclaredName :: !ModulePath
    -- ^ The name the cabal stanza (or, as a fallback, the file path)
    -- expected.  Only used to detect and report a disagreement — the
    -- parse tree is the authority.
  , msPath         :: !FilePath
  , msVisibility   :: !Visibility
  , msContent      :: !Text
  }

data ComponentIndex = ComponentIndex
  { ciRows          :: ![IndexRow]
  , ciParseFailures :: ![(ModulePath, Parser.ParseError)]
  , ciNameMismatch  :: ![(ModulePath, ModulePath)]
    -- ^ @(name the stanza expected, name the source declares)@.
  }

-- | Build a component's rows.  Pure, because every judgement in here —
-- which module a symbol is defined in, which signature it carries, what
-- the module is called — is a function of the sources, and mixing that
-- with file IO is what let the old indexer paper over parse failures.
indexComponentPure
  :: ComponentKey
  -> Extensions.LanguageSettings
  -> [ModuleSource]
  -> ComponentIndex
indexComponentPure compKey ls sources = ComponentIndex
  { ciRows          = rows
  , ciParseFailures = failures
  , ciNameMismatch  = mismatches
  }
  where
    parsed =
      [ (ms, Interface.parseInterface ls (msPath ms) (msContent ms))
      | ms <- sources
      ]

    failures =
      [ (msDeclaredName ms, e) | (ms, Left e) <- parsed ]

    ok = [ (ms, i) | (ms, Right i) <- parsed ]

    mismatches =
      [ (msDeclaredName ms, miName i)
      | (ms, i) <- ok
      , msDeclaredName ms /= miName i
      ]

    ifaces = map snd ok

    -- Visibility is a property of the module, so key it by the name the
    -- source declares — the same key the resolution map uses.
    visibilityOf = Map.fromList [ (miName i, msVisibility ms) | (ms, i) <- ok ]
    ifaceOf      = Map.fromList [ (miName i, i)               | (_,  i) <- ok ]

    rows =
      [ IndexRow
          { rowComponent  = compKey
          , rowModule     = presented
          , rowName       = name
          , rowSignature  = sig
          , rowDefModule  = defMod
          , rowVisibility = Map.findWithDefault Internal presented visibilityOf
          }
      | ((presented, name), res) <- Map.toList (Reexport.resolveComponent ifaces)
      , not (isDefinedOutside (Reexport.resSite res))
        -- No signature to give and the definition belongs to another
        -- index entry: a row here is what invented
        -- @Data.IntMap.Lazy.insertWith :: … Map k a@.
      , let defMod = Reexport.definitionModule presented (resSite res)
      , Just defIface <- [Map.lookup defMod ifaceOf]
      , Just decl <- [Parser.findDecl (unSymbolName name) (miDecls defIface)]
      , let sig = Signature (fromMaybe "" (Parser.declSigText
                              (sourceOf defMod) decl))
      ]

    -- The definition module's own bytes, for slicing the signature.
    sourceOf m = Map.findWithDefault "" m
      (Map.fromList [ (miName i, msContent ms) | (ms, i) <- ok ])

isDefinedOutside :: Reexport.DefinitionSite -> Bool
isDefinedOutside = \case
  Reexport.DefinedOutside{} -> True
  Reexport.DefinedHere      -> False
  Reexport.DefinedIn{}      -> False
```

Two deliberate choices to preserve:

- A module with no explicit export list contributes every declared name.
  `Interface.interfaceExportedNames` already falls back to
  `declaredNames`, so this needs no special case here — over-inclusion is
  right for a search index and matches today's behaviour.
- `Parser.findDecl` is looked up in the **definition** module's decls,
  never in a component-wide name map. That single change is what fixes
  the wrong-signature rows; a reviewer should be able to point at this
  line for it.

- [ ] **Step 5: Rewrite the IO shell**

`indexComponent` becomes: read each module named by `ciExposedModules` (→ `Exposed`) and `ciOtherModules` (→ `Internal`), resolving each to a file under `ciHsSourceDirs`; feed `indexComponentPure`; write rows; report failures and mismatches to stderr with the component key. `enumModulesIn`'s filesystem walk survives **only** for components whose cabal yielded no module lists, and rows produced that way are logged once per component:

```haskell
      hPutStrLn stderr $
        "hypha index: " <> Text.unpack (unComponentKey compKey)
          <> " has no cabal module list; falling back to a source-dir walk"
```

- [ ] **Step 6: Run everything**

Run: `cabal build all && cabal test all`
Expected: PASS. `Golden/Server.hs` may shift: modules that were previously named after paths now carry real names. Inspect each diff and accept only improvements.

- [ ] **Step 7: Commit**

```bash
git add src/Hypha/Search/Index.hs test/Unit/SearchIndexBuild.hs hypha.cabal test/Main.hs
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "fix(search): name modules from the parse tree, resolve sigs at the definition"
```

---

### Task 10: Search entities — package and module rows, kind-aware ranking

**Files:**
- Modify: `src/Hypha/Search/Fuzzy.hs`
- Modify: `src/Hypha/Search/Index.hs` (synthesise entity rows on both paths)
- Create: `test/Property/SearchRanking.hs`
- Modify: `hypha.cabal`, `test/Main.hs`

**Interfaces:**
- Consumes: `IndexRow`, `Visibility` (Task 7).
- Produces:
  ```haskell
  data Entity
    = EntityPackage !PackageName !Version
    | EntityModule  !ComponentKey !ModulePath !Visibility
    | EntitySymbol  !IndexRow
  data ResultKind = KindPackage | KindModule | KindSymbol
  entityKind :: Entity -> ResultKind
  data IndexedRow = IndexedRow
    { irEntity  :: !Entity            -- ^ typed payload, read at the render edge
    , irPkgL    :: !Text              -- ^ lowercased match fields
    , irModL    :: !Text
    , irNameL   :: !Text
    , irQualL   :: !Text
    , irNameLen :: !Int
    }
  mkSymbolRow  :: IndexRow -> IndexedRow
  mkPackageRow :: PackageName -> Version -> IndexedRow
  mkModuleRow  :: ComponentKey -> ModulePath -> Visibility -> IndexedRow
  entityRows   :: [IndexRow] -> [IndexedRow]   -- ^ package + module rows for a component
  ```
  The typed payload sits beside the precomputed lowercase fields rather
  than being flattened into `Text`: scoring only ever touches the
  lowercase fields (so it stays allocation-free per keystroke), and
  `Hypha.Search.Collapse` reads `irEntity` directly instead of
  re-wrapping `Text` back into `ModulePath`/`ComponentKey` at the render
  edge. `displayRow`'s 4-tuple is deleted along the way.

- [ ] **Step 1: Write the failing property test**

Create `test/Property/SearchRanking.hs`, matching the falsify idiom of `test/Property/LookupCascade.hs` (`gen`, `assert`, `P.eq P..$`):

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- | Ranking properties: an exact package or module name outranks the
-- symbols underneath it, whatever else is in the index.
module Property.SearchRanking (tests) where

import Data.List (sortOn)
import Data.Ord  (Down (..))
import qualified Data.Text as Text

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import Test.Falsify.Property (assert, gen)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.Fuzzy
  ( Entity (..), IndexedRow, ResultKind (..), entityKind, irEntity
  , mkModuleRow, mkPackageRow, mkSymbolRow, scoreRow, tokenize )
import Hypha.Search.Index (IndexRow (..), Visibility (..))
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))

-- | Ranking as the server does it (Command.Server's scHumanSearch).
rankRows :: [Text.Text] -> [IndexedRow] -> [IndexedRow]
rankRows tokens rows =
  map snd (sortOn (Down . fst)
    [ (s, r) | r <- rows, Just s <- [scoreRow tokens r] ])

symbolRow :: Text.Text -> Text.Text -> Text.Text -> IndexRow
symbolRow pkg modPath name = IndexRow
  { rowComponent  = ComponentKey pkg
  , rowModule     = ModulePath modPath
  , rowName       = SymbolName name
  , rowSignature  = Signature (name <> " :: Int")
  , rowDefModule  = ModulePath modPath
  , rowVisibility = Exposed
  }

firstKind :: [IndexedRow] -> Maybe ResultKind
firstKind rows = case rows of
  (r : _) -> Just (entityKind (irEntity r))
  []      -> Nothing

tests :: TestTree
tests = testGroup "Property.SearchRanking"
  [ testCase "query 'containers' puts the package first" $ do
      let rows = mkPackageRow (PackageName "containers") (Version "0.7")
               : map mkSymbolRow
                   [ symbolRow "containers" "Data.Map.Strict" "insertWith"
                   , symbolRow "containers" "Data.Map.Strict" "containers"
                   ]
      firstKind (rankRows (tokenize "containers") rows) @?= Just KindPackage

  , testCase "query 'Data.Map.Strict' puts the module first" $ do
      let rows = mkModuleRow (ComponentKey "containers")
                             (ModulePath "Data.Map.Strict") Exposed
               : map mkSymbolRow
                   [ symbolRow "containers" "Data.Map.Strict" "insertWith" ]
      firstKind (rankRows (tokenize "Data.Map.Strict") rows) @?= Just KindModule

  , testProperty "an exact package name outranks any number of its symbols" $ do
      n   <- gen (Gen.inRange (Gen.between (1, 50)))
      pkg <- gen (Gen.elements ["containers", "aeson", "text", "hypha"])
      let syms = [ symbolRow pkg "Some.Module" (pkg <> Text.pack (show i))
                 | i <- [1 .. n] ]
          rows = mkPackageRow (PackageName pkg) (Version "1.0")
                   : map mkSymbolRow syms
          got  = firstKind (rankRows (tokenize pkg) rows)
      assert (P.eq P..$ ("expected", Just KindPackage)
                   P..$ ("actual", got))

  , testProperty "a two-token query is a symbol query, not a module query" $ do
      -- 'Data.Map insertWith' names a module and a symbol; the user
      -- wants the symbol.  Only single-token exact matches get the
      -- entity bonus.
      name <- gen (Gen.elements ["insertWith", "lookup", "alter"])
      let rows = mkModuleRow (ComponentKey "containers")
                             (ModulePath "Data.Map") Exposed
               : map mkSymbolRow [ symbolRow "containers" "Data.Map" name ]
          got  = firstKind (rankRows (tokenize ("Data.Map " <> name)) rows)
      assert (P.eq P..$ ("expected", Just KindSymbol)
                   P..$ ("actual", got))
  ]
```

Check `Gen.inRange`/`Gen.between`'s exact spelling against the `falsify` version pinned in `hypha.cabal` before assuming it compiles; `Gen.elements` is already used by `Property/LookupCascade.hs`, so copy from there if the range generator differs.

- [ ] **Step 2: Run it, verify it fails**

Run: `cabal test all --test-options='-p SearchRanking'`
Expected: FAIL — `ResultKind` not in scope.

- [ ] **Step 3: Add the kind and the bonus**

In `Fuzzy.hs`, add `Entity`, `ResultKind`, `entityKind`, restructure `IndexedRow` around `irEntity`, and extend `scoreRow`'s result with two terms:

```haskell
scoreRow :: [Text] -> IndexedRow -> Maybe Int
scoreRow []     _ = Nothing
scoreRow tokens r = do
  base <- foldTokens tokens r          -- unchanged field scoring
  pure (base + nameBonus (irNameLen r)
             + kindBonus tokens r
             + visibilityBonus r)

-- | Entity kinds outrank field scores outright: a query that names a
-- package wants the package, not one of its ten thousand symbols.  The
-- bonus exceeds any reachable accumulation of field scores (the ceiling
-- is 1000 per token plus a 60-point name bonus), so this is a tier, not
-- a nudge.
--
-- Single-token exactness is deliberate.  @Data.Map insertWith@ names a
-- module in its first token but is a symbol query; only a query that is
-- /nothing but/ the entity's name asks for the entity itself.
kindBonus :: [Text] -> IndexedRow -> Int
kindBonus tokens r = case entityKind (irEntity r) of
  KindPackage | [t] <- tokens, t == irPkgL r -> 100000
  KindModule  | [t] <- tokens, t == irModL r -> 50000
  _                                          -> 0

-- | A public presentation of a symbol never ties with an internal one.
-- Small, because it breaks ties rather than reordering tiers — collapse
-- (Task 11) is what actually folds the internal row away.
visibilityBonus :: IndexedRow -> Int
visibilityBonus r = case irEntity r of
  EntitySymbol row              -> vis (rowVisibility row)
  EntityModule _ _ v            -> vis v
  EntityPackage _ _             -> 0
  where
    vis Exposed  = 20
    vis Internal = 0
```

`entityKind` is a three-line total function; keep `ResultKind` derived from `Entity` rather than stored beside it, so the two cannot disagree.

- [ ] **Step 4: Synthesise entity rows**

In `Fuzzy.hs`:

```haskell
-- | The package and module rows a set of symbol rows implies.
--
-- Synthesised rather than stored: they are a projection of rows we
-- already have, and deriving them in one place is what stops the
-- freshly-built index and the hydrated-from-cache index from
-- disagreeing about which entities exist.  A module contributes one row
-- even when several of its symbols do.
entityRows :: PackageName -> Version -> [IndexRow] -> [IndexedRow]
entityRows pkg ver rows =
  mkPackageRow pkg ver
    : [ mkModuleRow c m v
      | (c, m, v) <- nubOrd [ (rowComponent r, rowModule r, rowVisibility r)
                            | r <- rows
                            ]
      ]
```

Both call sites in `Search/Index.hs` — `buildAndCacheIndex` after writing a component's rows, and `hydrateFromCache` after reading them — prepend `entityRows pkgName pkgVersion rows` to the same `IndexedRow` list they already build from `mkSymbolRow`. Use `Data.Containers.ListUtils.nubOrd`, not `nub`: a large package has thousands of rows per module.

- [ ] **Step 5: Run the tests**

Run: `cabal build all && cabal test all`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add src/Hypha/Search/Fuzzy.hs src/Hypha/Search/Index.hs \
        test/Property/SearchRanking.hs hypha.cabal test/Main.hs
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "feat(search): rank packages and modules as first-class results"
```

---

### Task 11: Collapse by definition, render `SearchResult`

**Files:**
- Create: `src/Hypha/Search/Collapse.hs`
- Modify: `src/Hypha/Server/Ui/Search.hs:67-78`, `src/Hypha/Server/App.hs:133-144`, `src/Hypha/Command/Server.hs:223-235`
- Create: `test/Unit/SearchCollapse.hs`
- Modify: `test/Golden/Server.hs`, `hypha.cabal`, `test/Main.hs`
- Modify: `assets/` CSS for the `+N` affordance (find with `grep -rn 'pkgmod' assets`)

**Interfaces:**
- Consumes: `IndexedRow`, `ResultKind` (Task 10).
- Produces:
  ```haskell
  data SearchResult
    = ResultPackage !PackageName !Version
    | ResultModule  !ComponentKey !ModulePath !Visibility
    | ResultSymbol  !SymbolResult
  data SymbolResult = SymbolResult
    { srComponent  :: !ComponentKey
    , srModule     :: !ModulePath
    , srName       :: !SymbolName
    , srSignature  :: !Signature
    , srDefModule  :: !ModulePath
    , srAlternates :: !Int
    }
  rankRows      :: [Text] -> [IndexedRow] -> [IndexedRow]
  collapseRows  :: [IndexedRow] -> [SearchResult]
  resultHref    :: SearchResult -> Text
  definitionHref :: SymbolResult -> Text
  ```

- [ ] **Step 1: Write the failing test**

Create `test/Unit/SearchCollapse.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for collapse: which of a symbol's presentations wins, and
-- which pairs must never be merged.
module Unit.SearchCollapse (tests) where

import Data.Text (Text)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.Collapse
  ( SearchResult (..), SymbolResult (..), collapseRows, resultHref )
import Hypha.Search.Fuzzy (mkSymbolRow)
import Hypha.Search.Index (IndexRow (..), Visibility (..))
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))

mapRow :: Text -> Text -> Visibility -> IndexRow
mapRow presented defined vis = IndexRow
  { rowComponent  = ComponentKey "containers"
  , rowModule     = ModulePath presented
  , rowName       = SymbolName "insertWith"
  , rowSignature  = Signature "insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a"
  , rowDefModule  = ModulePath defined
  , rowVisibility = vis
  }

tests :: TestTree
tests = testGroup "Unit.SearchCollapse"
  [ testCase "the wrapper wins over its .Internal definition site" $ do
      let rows = map mkSymbolRow
            [ mapRow "Data.Map.Strict.Internal" "Data.Map.Strict.Internal" Exposed
            , mapRow "Data.Map.Strict"          "Data.Map.Strict.Internal" Exposed
            ]
      case collapseRows rows of
        [ResultSymbol s] -> do
          srModule s     @?= ModulePath "Data.Map.Strict"
          srDefModule s  @?= ModulePath "Data.Map.Strict.Internal"
          srAlternates s @?= 1
        other -> fail ("expected one collapsed result, got " <> show (length other))

  , testCase "strict and lazy stay two results (same name, same sig)" $ do
      -- Both are insertWith with identical signatures.  Collapsing by
      -- name — or by name and signature — would merge them.  They differ
      -- only in definition site, which is why that is the key.
      let rows = map mkSymbolRow
            [ mapRow "Data.Map.Strict" "Data.Map.Strict.Internal" Exposed
            , mapRow "Data.Map.Lazy"   "Data.Map.Internal"        Exposed
            ]
      length (collapseRows rows) @?= 2

  , testCase "Exposed beats Internal when both present the definition" $ do
      let rows = map mkSymbolRow
            [ mapRow "Data.Map.Hidden" "Data.Map.Internal" Internal
            , mapRow "Data.Map"        "Data.Map.Internal" Exposed
            ]
      case collapseRows rows of
        [ResultSymbol s] -> srModule s @?= ModulePath "Data.Map"
        _ -> fail "expected one collapsed result"

  , testCase "shorter path breaks a tie between two exposed wrappers" $ do
      let rows = map mkSymbolRow
            [ mapRow "Data.Map.Strict.Extra" "Data.Map.Internal" Exposed
            , mapRow "Data.Map"              "Data.Map.Internal" Exposed
            ]
      case collapseRows rows of
        [ResultSymbol s] -> srModule s @?= ModulePath "Data.Map"
        _ -> fail "expected one collapsed result"

  , testCase "hrefs point at the presentation, not the definition" $ do
      let rows = [mkSymbolRow (mapRow "Data.Map.Strict" "Data.Map.Strict.Internal" Exposed)]
      case collapseRows rows of
        [r@(ResultSymbol _)] ->
          resultHref r @?= "/pkg/containers/Data.Map.Strict/insertWith"
        _ -> fail "expected one result"
  ]
```

- [ ] **Step 2: Run it, verify it fails**

Run: `cabal test all --test-options='-p Unit.SearchCollapse'`
Expected: FAIL — `Hypha.Search.Collapse` does not exist.

- [ ] **Step 3: Write `Hypha.Search.Collapse`**

```haskell
-- | Fold every presentation of one definition into a single result.
--
-- The group key is @(component, definition module, name)@ — not the
-- name, and not the name plus signature.  @Data.Map.Strict.insertWith@
-- and @Data.Map.Lazy.insertWith@ have the same name /and/ the same
-- signature and are different functions; they differ only in where they
-- are defined, which is why that is the key.
collapseRows :: [IndexedRow] -> [SearchResult]
collapseRows rows = map render (groupsInOrder symbolRows) ++ others
  where
    symbolRows = [ (key r, r) | r <- rows, EntitySymbol{} <- [irEntity r] ]
    others     = [ entityResult (irEntity r) | r <- rows, notSymbol (irEntity r) ]

    key r = case irEntity r of
      EntitySymbol row -> ( rowComponent row, rowDefModule row, rowName row )
      _                -> error "unreachable"   -- see note below

    render (_, group) =
      let winner = minimumBy (comparing presentationRank) group
      in ResultSymbol (symbolResult winner (length group - 1))

-- | Which presentation of a definition the user should land on.
-- Ordered: exposed before internal, then a path with no @Internal@
-- segment, then fewer segments, then lexicographic.  Total and
-- deterministic, so the winner does not depend on the order SQLite
-- happened to return rows in.
presentationRank :: IndexRow -> (Int, Int, Int, Text)
presentationRank row =
  ( case rowVisibility row of Exposed -> 0; Internal -> 1
  , if any (== "Internal") segments then 1 else 0
  , length segments
  , unModulePath (rowModule row)
  )
  where segments = Text.splitOn "." (unModulePath (rowModule row))

resultHref :: SearchResult -> Text
resultHref = \case
  ResultPackage p _  -> "/pkg/" <> unPackageName p
  ResultModule c m _ -> "/pkg/" <> unComponentKey c <> "/" <> unModulePath m
  ResultSymbol s     -> "/pkg/" <> unComponentKey (srComponent s)
                          <> "/" <> unModulePath (srModule s)
                          <> "/" <> unSymbolName (srName s)

-- | Where the @+N@ chip points: the definition site, so the escape
-- hatch from a collapsed group is one click.
definitionHref :: SymbolResult -> Text
definitionHref s =
  "/pkg/" <> unComponentKey (srComponent s)
    <> "/" <> unModulePath (srDefModule s)
    <> "/" <> unSymbolName (srName s)
```

The `error "unreachable"` in the sketch above is **not acceptable** in the final code — it is exactly the smell CLAUDE.md forbids. Restructure so the impossible branch cannot be written: partition once, carrying the `IndexRow` out of the `Entity` as you go, so the grouping function only ever sees `IndexRow`s:

```haskell
collapseRows :: [IndexedRow] -> [SearchResult]
collapseRows rows = interleave (map classify rows)
  where
    classify r = case irEntity r of
      EntitySymbol row   -> Left row
      EntityPackage p v  -> Right (ResultPackage p v)
      EntityModule c m v -> Right (ResultModule c m v)
```

then group the `Left`s by key, preserving first-appearance order (the input arrives already ranked), and splice each group's single result back where its first member sat.

- [ ] **Step 3b: Add the collapse property**

Append to `test/Property/SearchRanking.hs`:

```haskell
  , testProperty "collapse preserves definition keys and drops nothing" $ do
      -- Two invariants at once: every distinct (component, defModule,
      -- name) survives collapse exactly once, and no name disappears.
      n <- gen (Gen.inRange (Gen.between (1, 20)))
      let defs   = [ ("containers", "Data.Map.Internal", "sym" <> Text.pack (show i))
                   | i <- [1 .. n] ]
          rows   = concat
            [ [ mkSymbolRow (presented c d s "Data.Map")
              , mkSymbolRow (presented c d s d)
              ]
            | (c, d, s) <- defs
            ]
          keys   = [ (c, d, s) | (c, d, s) <- defs ]
          got    = [ (unComponentKey (srComponent r), unModulePath (srDefModule r),
                      unSymbolName (srName r))
                   | ResultSymbol r <- collapseRows rows
                   ]
      assert (P.eq P..$ ("expected", keys) P..$ ("actual", got))
```

with a local `presented component defModule name presentationModule :: IndexRow` helper mirroring `symbolRow`. Every pair collapses to one result, so `got` has exactly `n` entries in input order.

- [ ] **Step 4: Render it**

`Ui/Search.hs`'s `resultsFragment :: [Text] -> [SearchResult] -> Html ()` renders per constructor: a package row shows the version, a module row shows its component, a symbol row keeps today's name/sig/pkgmod layout plus, when `srAlternates > 0`, a `+N` chip linking `definitionHref`:

```haskell
        span_ [class_ "alt-count", title_ ("also in " <> Text.pack (show n) <> " internal module(s)")]
          (toHtml ("+" <> Text.pack (show n)))
```

Add `.alt-count` styling next to `.pkgmod` in the stylesheet. `App.searchPage` and `Command/Server.hs`'s `scHumanSearch` change type from `[(Text,Text,Text,Text)]` to `[SearchResult]`, with `scopeSearchRows` filtering on `srComponent`/`ResultPackage` name.

- [ ] **Step 5: Run everything, update the server golden**

Run: `cabal build all && cabal test all`
Expected: `Golden/Server.hs` diffs (the results markup changed). Read the diff, confirm each change is the intended new markup, then refresh with `cabal test all --test-options=--accept`.

- [ ] **Step 6: Commit**

```bash
git add src/Hypha/Search/Collapse.hs src/Hypha/Server/Ui/Search.hs src/Hypha/Server/App.hs \
        src/Hypha/Command/Server.hs test/Unit/SearchCollapse.hs test/Golden/Server.hs \
        test/Golden/golden assets hypha.cabal test/Main.hs
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "feat(search): collapse a symbol to its most public presentation"
```

---

### Task 12: Locate — typed failures, resolved definitions, no sweeps

**Files:**
- Modify: `src/Hypha/Source/Locate.hs:251-330` (`locateSymbolDefinitionInDir`, `findInTree`, delete `sortByPrefix`/`sortBy`/`modulePrefix`), `:363-379` (`scanFile`)
- Modify: `src/Hypha/Command/Source.hs:113-116`
- Modify: `test/Unit/SourceExtract.hs` or a new `test/Unit/SourceLocate.hs`

**Interfaces:**
- Consumes: `parseInterface` (Task 4), `resolveComponent` (Task 5), `ParseError` (Task 2).
- Produces:
  ```haskell
  data Provenance = Resolved !DefinitionSite | GuessedBySweep !Text
  data LocatedDefinition = LocatedDefinition
    { ldLocation   :: !SourceLocation
    , ldModule     :: !ModulePath
    , ldProvenance :: !Provenance
    }
  scanFile :: SymbolName -> FilePath -> IO (Either ParseError (Maybe SourceLocation))
  locateDefinitionInComponent
    :: [ModuleSource] -> ModulePath -> SymbolName -> IO (Maybe LocatedDefinition)
  ```

- [ ] **Step 1: Write the failing test**

Create `test/Unit/SourceLocate.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for definition location.
--
-- The regression: @hypha source containers/Data.Map.Internal/balanceL@
-- reported @Data/Set/Internal.hs@.  Two defects stacked — a parse
-- failure returned as @Nothing@ (indistinguishable from "not here"), and
-- a package-wide sweep ranked by shared /characters/ between a dotted
-- module name and a slashed file path.
module Unit.SourceLocate (tests) where

import           Data.List (isSuffixOf)
import qualified Data.Text as Text

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Search.Reexport (DefinitionSite (..))
import Hypha.Source.Locate
  ( LocatedDefinition (..), Provenance (..), SourceLocation (..)
  , locateDefinitionInComponent, scanFile )
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))
import Util.Fixture (fixtureSources)

locate :: Text.Text -> Text.Text -> IO LocatedDefinition
locate modPath sym = do
  srcs <- fixtureSources
  mLd  <- locateDefinitionInComponent srcs (ModulePath modPath) (SymbolName sym)
  maybe (fail ("no definition found for " <> Text.unpack modPath
                 <> "/" <> Text.unpack sym))
        pure
        mLd

tests :: TestTree
tests = testGroup "Unit.SourceLocate"
  [ testCase "a locally declared symbol resolves in its own module" $ do
      ld <- locate "Fixture.Internal" "insertBag"
      ldModule ld @?= ModulePath "Fixture.Internal"
      ldProvenance ld @?= Resolved DefinedHere
      assertBool "path is Fixture/Internal.hs"
        ("Fixture/Internal.hs" `isSuffixOf` slPath (ldLocation ld))

  , testCase "a re-export resolves to its definition, not a swept sibling" $ do
      ld <- locate "Fixture.Wrapper" "insertBag"
      ldModule ld @?= ModulePath "Fixture.Internal"
      ldProvenance ld @?= Resolved (DefinedIn (ModulePath "Fixture.Internal"))

  , testCase "a same-named sibling is not mistaken for the definition" $ do
      -- Fixture.Internal and Fixture.StrictInternal both declare
      -- insertBag.  Fixture.Strict imports the strict one, and that
      -- import is what decides it — not file order.
      ld <- locate "Fixture.Strict" "insertBag"
      ldModule ld @?= ModulePath "Fixture.StrictInternal"
      assertBool "path is Fixture/StrictInternal.hs"
        ("Fixture/StrictInternal.hs" `isSuffixOf` slPath (ldLocation ld))

  , testCase "scanFile reports a parse failure instead of 'not found'" $ do
      r <- scanFile (SymbolName "anything")
             "test/fixtures/reexport/src/Fixture/Broken.hs"
      case r of
        Left e  -> assertBool "message is non-empty"
                     (not (Text.null (peMessage e)))
        Right _ -> fail "expected Left for an unparsable module"

  , testCase "scanFile returns Right Nothing when the symbol is absent" $ do
      r <- scanFile (SymbolName "insertBag")
             "test/fixtures/reexport/src/Fixture/Other.hs"
      r @?= Right Nothing
  ]
```

Add the deliberately broken fixture `test/fixtures/reexport/src/Fixture/Broken.hs` (not listed in the fixture cabal file, so it never enters the index fixtures):

```haskell
module Fixture.Broken (f) where

f x = case x of
  -> 1
```

Import `peMessage` from `Hypha.Source.Parser`.

- [ ] **Step 2: Run it, verify it fails**

Run: `cabal test all --test-options='-p Unit.SourceLocate'`
Expected: FAIL — `scanFile` has the old type; `locateDefinitionInComponent` missing.

- [ ] **Step 3: Rewrite the locate path**

```haskell
-- | Where a symbol is declared, and how confident we are about it.
--
-- The @GuessedBySweep@ arm exists so a fallback can never be mistaken
-- for a resolution.  Its predecessor returned the same 'SourceLocation'
-- either way, which is how @Data.Map.Internal/balanceL@ came to report
-- @Data/Set/Internal.hs@ as if it were fact.
data Provenance
  = Resolved !DefinitionSite
  | GuessedBySweep !Text          -- ^ why resolution was unavailable
  deriving stock (Show, Eq)

data LocatedDefinition = LocatedDefinition
  { ldLocation   :: !SourceLocation
  , ldModule     :: !ModulePath
  , ldProvenance :: !Provenance
  }
  deriving stock (Show, Eq)

-- | Parse-tree lookup of one symbol in one file.
--
-- The three outcomes are distinct on purpose: @Left@ means we could not
-- read the module, @Right Nothing@ means we read it and the symbol is
-- not declared there, @Right (Just loc)@ means we found it.  Collapsing
-- the first two into @Nothing@ — which is what this function used to do
-- — turns "unreadable module" into "look somewhere else", and the
-- somewhere else is wrong.
scanFile :: SymbolName -> FilePath -> IO (Either ParseError (Maybe SourceLocation))
scanFile sym f = do
  src <- TIO.readFile f
  pure $ case Parser.parseDecls f src of
    Left e      -> Left e
    Right decls -> Right $ do
      d    <- Parser.findDecl (unSymbolName sym) decls
      line <- Parser.declDefLine d <|> Parser.declSigLine d
      pure (SourceLocation f line)

-- | Locate a symbol given the component's modules.  Resolution first,
-- sweep never: with the component in hand there is nothing to guess.
locateDefinitionInComponent
  :: [ModuleSource] -> ModulePath -> SymbolName -> IO (Maybe LocatedDefinition)
locateDefinitionInComponent sources asking sym = do
  ifaces <- pure (rights [ parseInterface defaults (msPath s) (msContent s)
                         | s <- sources ])
  let resolution = Map.lookup (asking, sym) (resolveComponent ifaces)
  case resolution of
    Nothing -> do
      hPutStrLn stderr $
        "hypha: " <> Text.unpack (unModulePath asking) <> " does not export "
          <> Text.unpack (unSymbolName sym)
      pure Nothing
    Just res -> do
      let target = definitionModule asking (resSite res)
      case [ s | s <- sources, msDeclaredName s == target ] of
        []      -> pure Nothing        -- DefinedOutside: another index entry's job
        (s : _) -> do
          scanned <- scanFile sym (msPath s)
          case scanned of
            Left e -> do
              hPutStrLn stderr $
                "hypha: " <> msPath s <> " could not be parsed: "
                  <> Text.unpack (peMessage e)
              pure Nothing
            Right Nothing    -> pure Nothing
            Right (Just loc) -> pure (Just (LocatedDefinition
              { ldLocation   = loc
              , ldModule     = target
              , ldProvenance = Resolved (resSite res)
              }))
```

- `findInTree` survives **only** behind `GuessedBySweep`, for callers
  with no component information (a package whose cabal we could not
  parse). Its ranking uses `sharedSegments` on `ModulePath`s (Task 5),
  comparing segments of the *module name* derived from each candidate
  file rather than characters of its path. Delete `sortByPrefix`, the
  hand-rolled local `sortBy`, and `modulePrefix`.
- `locateSymbolDefinitionInDir` keeps its name and signature for callers
  that only have a directory, and is reimplemented as: resolve when the
  cabal gives us modules, sweep otherwise.
- `Command/Source.hs`'s `locateSourceLoc` calls
  `locateDefinitionInComponent` when component dirs are available, and
  prints `GuessedBySweep`'s reason to stderr when the sweep fired, so a
  guessed path is never silently presented as a resolved one.

- [ ] **Step 4: Verify the reported cases by hand**

Run:
```bash
cabal run hypha -- source 'containers/Data.Map.Internal/balanceL'
cabal run hypha -- source 'containers/Data.Map.Internal/insertWith'
cabal run hypha -- source 'containers/Data.Map.Strict/insertWith'
```
Expected: the first two report `path: …/src/Data/Map/Internal.hs`; the third reports `…/src/Data/Map/Strict/Internal.hs` (its real definition site). Before the fix these reported `Data/Set/Internal.hs` and `Data/Map/Strict/Internal.hs`.

- [ ] **Step 5: Run everything and commit**

Run: `cabal build all && cabal test all`

```bash
git add src/Hypha/Source/Locate.hs src/Hypha/Command/Source.hs \
        test/Unit/SourceLocate.hs hypha.cabal test/Main.hs
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "fix(source): locate definitions by resolution, not by sweeping the package"
```

---

### Task 13: Symbol card reports what it resolved

**Files:**
- Modify: `src/Hypha/Server/ModuleDoc.hs:56-70` (`SymbolCardData`)
- Modify: `src/Hypha/Command/Server.hs:236-275` (`scSymbolLookup`; delete `modulePathFromFile`)
- Modify: `src/Hypha/Server/Ui/Doc.hs` (card rendering)
- Modify: `test/Unit/Server.hs`, `test/Golden/Server.hs`

**Interfaces:**
- Consumes: `LocatedDefinition`, `Provenance` (Task 12).
- Produces:
  ```haskell
  data SymbolCardData = SymbolCardData
    { scdSignature  :: !(Maybe Signature)
    , scdHaddock    :: !(Maybe DocText)
    , scdModule     :: !ModulePath      -- ^ definition site
    , scdRequested  :: !ModulePath      -- ^ what the URL asked for
    , scdProvenance :: !Provenance
    , scdLine       :: !(Maybe SrcLine)
    , scdKind       :: !(Maybe DeclKind)
    }
  ```

- [ ] **Step 1: Write the failing test**

In `test/Unit/Server.hs` (it already renders `Html` to `Text` for `hackageLink`; reuse that helper):

```haskell
symbolCardTests :: [TestTree]
symbolCardTests =
  [ testCase "a re-exported symbol names both modules" $ do
      let html = renderCard SymbolCardData
            { scdSignature  = Just (Signature "insertWith :: Ord k => …")
            , scdHaddock    = Nothing
            , scdModule     = ModulePath "Data.Map.Strict.Internal"
            , scdRequested  = ModulePath "Data.Map.Strict"
            , scdProvenance = Resolved (DefinedIn (ModulePath "Data.Map.Strict.Internal"))
            , scdLine       = Just (SrcLine 552)
            , scdKind       = Just DkFunction
            }
      assertBool "mentions the presentation module"
        ("Data.Map.Strict" `Text.isInfixOf` html)
      assertBool "mentions the definition module"
        ("Data.Map.Strict.Internal" `Text.isInfixOf` html)

  , testCase "a locally defined symbol does not claim a re-export" $ do
      let html = renderCard SymbolCardData
            { scdSignature  = Just (Signature "insertWith :: Ord k => …")
            , scdHaddock    = Nothing
            , scdModule     = ModulePath "Data.Map.Internal"
            , scdRequested  = ModulePath "Data.Map.Internal"
            , scdProvenance = Resolved DefinedHere
            , scdLine       = Just (SrcLine 552)
            , scdKind       = Just DkFunction
            }
      assertBool "no re-export line"
        (not ("re-exported" `Text.isInfixOf` html))

  , testCase "a missing signature says so instead of rendering blank" $ do
      let html = renderCard SymbolCardData
            { scdSignature  = Nothing
            , scdHaddock    = Nothing
            , scdModule     = ModulePath "Data.Map.Internal"
            , scdRequested  = ModulePath "Data.Map.Internal"
            , scdProvenance = Resolved DefinedHere
            , scdLine       = Nothing
            , scdKind       = Nothing
            }
      assertBool "explains the absence"
        ("no signature" `Text.isInfixOf` Text.toLower html)

  , testCase "a swept location is labelled as a guess" $ do
      let html = renderCard SymbolCardData
            { scdSignature  = Just (Signature "balanceL :: …")
            , scdHaddock    = Nothing
            , scdModule     = ModulePath "Data.Set.Internal"
            , scdRequested  = ModulePath "Data.Map.Internal"
            , scdProvenance = GuessedBySweep "package cabal could not be parsed"
            , scdLine       = Just (SrcLine 1746)
            , scdKind       = Just DkFunction
            }
      assertBool "surfaces the uncertainty"
        (any (`Text.isInfixOf` Text.toLower html) ["best guess", "unverified", "swept"])
  ]
  where
    renderCard = LText.toStrict . renderText . symbolCard "insertWith" "containers"
```

The last case is the one that matters most for trust: before this task, a swept location rendered identically to a resolved one.

- [ ] **Step 2: Run it, verify it fails**

Run: `cabal test all --test-options='-p Unit.Server'`
Expected: FAIL — `scdRequested` not a field.

- [ ] **Step 3: Rewire `scSymbolLookup`**

The whole `(info, resolvedMod, lineOverride)` dance at `Command/Server.hs:259-266` goes away, because the question it was guessing at ("is the symbol really in this module?") now has an answer:

```haskell
    , App.scSymbolLookup = \pkgT modT symT -> do
        mSources <- componentSourcesFor plan resolver pkgT
        case mSources of
          Nothing      -> pure Nothing
          Just sources -> do
            mLd <- Locate.locateDefinitionInComponent sources
                     (ModulePath modT) (SymbolName symT)
            case mLd of
              Nothing -> pure Nothing
              Just ld -> do
                src <- TIO.readFile (Locate.slPath (Locate.ldLocation ld))
                let info = Extract.extractSymbolInfo src symT
                pure (Just SymbolCardData
                  { scdSignature  = Signature <$> Extract.siSignature info
                  , scdHaddock    = Extract.siHaddock info
                  , scdModule     = Locate.ldModule ld
                  , scdRequested  = ModulePath modT
                  , scdProvenance = Locate.ldProvenance ld
                  , scdLine       = Just (SrcLine (Locate.slLine (Locate.ldLocation ld)))
                  , scdKind       = Extract.siKind info
                  })
```

`componentSourcesFor` is `resolveComponentDirs` plus the module list, i.e. the same `[ModuleSource]` builder Task 9's IO shell already needs — export it from `Search.Index` and use one implementation, not two.

Delete `modulePathFromFile`: this was its last caller, and it is item 3's path-derived naming leaking onto a user-visible label. The `""` sentinels for signature and haddock become `Maybe`, so `Ui/Doc.hs` renders "no signature in the source" instead of an empty cream box.

- [ ] **Step 4: Run and update goldens**

Run: `cabal build all && cabal test all`, inspect `Golden/Server.hs` diffs, accept the intended markup with `--accept`.

- [ ] **Step 5: Commit**

```bash
git add src/Hypha/Server/ModuleDoc.hs src/Hypha/Command/Server.hs src/Hypha/Server/Ui/Doc.hs \
        test/Unit/Server.hs test/Golden/Server.hs test/Golden/golden
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "fix(server): label the symbol card with the resolved definition module"
```

---

### Task 14: Module page lists re-exported entries

**Files:**
- Modify: `src/Hypha/Source/Extract.hs:84-95` (`DocEntry`), `:101-117` (`extractModuleDoc`)
- Modify: `src/Hypha/Command/Server.hs:344-425` (`moduleDocFor`, `filterByExports`)
- Modify: `src/Hypha/Server/Ui/ModuleDoc.hs:93,133-147` (entries + `tocRail`)
- Modify: `test/Unit/SourceExtract.hs`, `test/Golden/Server.hs`

**Interfaces:**
- Consumes: `resolveComponent` (Task 5), `IndexRow`s from the cache (Task 7), `ModuleSource` (Task 9).
- Produces:
  ```haskell
  data EntryOrigin = EntryLocal | EntryReexport !ModulePath
  -- DocEntry gains: deOrigin :: !EntryOrigin
  resolveModuleEntries
    :: [ModuleSource]          -- ^ the component's modules
    -> ModulePath              -- ^ the page's module
    -> Either ParseError [DocEntry]
  ```

- [ ] **Step 1: Write the failing test**

In `test/Unit/SourceExtract.hs`:

```haskell
  , testCase "a wrapper module's entries include its re-exports" $ do
      srcs <- fixtureSources   -- the Task 9 helper, shared via a test util
      case resolveModuleEntries srcs (ModulePath "Fixture.Wrapper") of
        Left e   -> fail ("unexpected parse error: " <> show e)
        Right es -> do
          let names = sort (map (unSymbolName . deName) es)
          names @?= ["Bag", "insertBag", "otherOnly", "sizeBag"]
          -- and each carries where it came from
          [ deOrigin e | e <- es, deName e == SymbolName "insertBag" ]
            @?= [EntryReexport (ModulePath "Fixture.Internal")]

  , testCase "a definition module's entries are all local" $ do
      srcs <- fixtureSources
      case resolveModuleEntries srcs (ModulePath "Fixture.Internal") of
        Left e   -> fail ("unexpected parse error: " <> show e)
        Right es -> assertBool "all local" (all ((== EntryLocal) . deOrigin) es)
```

`fixtureSources` comes from `test/Util/Fixture.hs` (Task 9, Step 1) — import it rather than rebuilding the source list here.

- [ ] **Step 2: Run it, verify it fails**

Run: `cabal test all --test-options='-p Unit.SourceExtract'`
Expected: FAIL — `resolveModuleEntries` missing, `DocEntry` has no `deOrigin`.

- [ ] **Step 3: Implement**

```haskell
-- | Where a module-page entry came from.  A wrapper module's page is
-- almost entirely re-exports, and telling the user which module actually
-- defines each entry is the difference between a useful page and a list
-- of names.
data EntryOrigin
  = EntryLocal
  | EntryReexport !ModulePath
  deriving stock (Show, Eq)

-- | Every entry a module's page should show, re-exports included.
--
-- The predecessor filtered locally-declared entries by the export list,
-- so a pure re-export module produced nothing — @Data.Map.Strict@ had an
-- empty \"On this page\" rail.  Resolving exports to their definitions
-- means the wrapper shows what Haddock shows.
resolveModuleEntries
  :: [ModuleSource]
  -> ModulePath
  -> Either Parser.ParseError [DocEntry]
resolveModuleEntries sources asking = do
  ifaces <- traverse parseOne sources
  let byName     = Map.fromList [ (miName i, i) | i <- ifaces ]
      contentOf  = Map.fromList [ (msDeclaredName s, msContent s) | s <- sources ]
      resolution = Reexport.resolveComponent ifaces
  asked <- maybe (Left (missingModule asking)) Right (Map.lookup asking byName)
  pure
    [ entry
    | name <- Interface.interfaceExportedNames asked
    , Just res <- [Map.lookup (asking, name) resolution]
    , let defMod = Reexport.definitionModule asking (resSite res)
    , Just defIface <- [Map.lookup defMod byName]
    , Just decl <- [Parser.findDecl (unSymbolName name) (miDecls defIface)]
    , let src = Map.findWithDefault "" defMod contentOf
    , let entry = (Extract.docEntryFor src decl)
            { deOrigin = if defMod == asking
                           then EntryLocal
                           else EntryReexport defMod
            }
    ]
  where
    parseOne s = Interface.parseInterface
                   Extensions.defaultLanguageSettings (msPath s) (msContent s)
```

`Extract.docEntryFor :: Text -> Decl -> DocEntry` is the existing per-declaration constructor inside `extractModuleDoc`, lifted to a top-level export so both paths build entries identically rather than each growing its own copy. `missingModule` is a `ParseError` value naming the module — not an `error`, because a URL can name a module the component does not have.

Entries keep source order per definition module; the export list's order drives the page, as it does today.

`moduleDocFor`'s `sourceView` calls it in place of `Extract.extractModuleDoc` + `filterByExports`. Two paths, in priority order, both reported in the view rather than silently chosen:
1. component modules from the cabal stanza (the common case);
2. demand-driven, following the requested module's imports up to a
   bounded depth, when no cabal module list exists. Reaching the bound
   is a `ViewFromSource` note, not a silent truncation.

`filterByExports`'s "empty result keeps everything" hack goes away: the export list is now resolved rather than intersected, so a pure re-export module yields entries instead of nothing.

`Ui/ModuleDoc.hs` renders `deOrigin` as a small "from `Fixture.Internal`" line on the entry, and `tocRail` now receives a non-empty list for wrapper modules.

- [ ] **Step 4: Verify the reported page by hand**

Run: `cabal run hypha -- server --port 4287` and open `/pkg/containers/Data.Map.Strict`.
Expected: the "On this page" rail lists `insertWith`, `lookup`, … each marked as coming from `Data.Map.Strict.Internal`. Note the sandbox blocks localhost requests from this session, so this step is the human's to run; report it as unverified if you cannot.

- [ ] **Step 5: Run everything and commit**

Run: `cabal build all && cabal test all`, refresh goldens after reading their diffs.

```bash
git add src/Hypha/Source/Extract.hs src/Hypha/Command/Server.hs \
        src/Hypha/Server/Ui/ModuleDoc.hs test/Unit/SourceExtract.hs test/Util/Fixture.hs \
        test/Golden/Server.hs test/Golden/golden hypha.cabal test/Main.hs
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "feat(server): list re-exported entries on wrapper module pages"
```

---

### Task 15: Hackage link for distribution packages

**Files:**
- Modify: `src/Hypha/Server/Ui/Tree.hs:106-119`
- Modify: `src/Hypha/Server/App.hs:160`
- Modify: `test/Unit/Server.hs:99-122`

**Interfaces:**
- Produces: `hackageLink :: PackageName -> Version -> PackageOrigin -> Html ()`

- [ ] **Step 1: Write the failing test**

Replace `hackageLinkTests` in `test/Unit/Server.hs` with a case per origin:

```haskell
hackageLinkTests :: [TestTree]
hackageLinkTests =
  [ testCase "Hackage origin links to the pinned version" $
      renderLink "aeson" "2.2.1.0" OriginHackage
        `shouldContain` "https://hackage.haskell.org/package/aeson-2.2.1.0"

  , testCase "distribution (boot) packages link too" $
      -- containers, base and every other boot library IS published on
      -- Hackage; withholding the link was the bug.
      renderLink "containers" "0.7" OriginDistribution
        `shouldContain` "https://hackage.haskell.org/package/containers-0.7"

  , testCase "local packages get no link" $
      renderLink "myapp" "0.1.0" (OriginLocal "/src/myapp") @?= ""

  , testCase "source-repository-package gets no link" $
      renderLink "forked" "1.0" (OriginSourceRepo Nothing Nothing Nothing) @?= ""

  , testCase "local tarball gets no link" $
      renderLink "tar" "1.0" (OriginLocalTarball "/t.tar.gz") @?= ""

  , testCase "remote tarball gets no link" $
      renderLink "tar" "1.0" (OriginRemoteTarball "https://x/t.tar.gz") @?= ""
  ]
  where
    renderLink pkg ver origin =
      LText.toStrict (renderText (hackageLink (PackageName pkg) (Version ver) origin))
    shouldContain hay needle = assertBool (show needle <> " in " <> show hay)
                                 (needle `Text.isInfixOf` hay)
```

- [ ] **Step 2: Run it, verify it fails**

Run: `cabal test all --test-options='-p Tree.hackageLink'`
Expected: FAIL — the distribution case renders `""`.

- [ ] **Step 3: Implement**

```haskell
-- | Right-aligned "view on Hackage" link for the package page header.
--
-- Hackage-sourced *and* distribution (boot) packages both get one:
-- @containers@, @base@ and every other library shipped with GHC is
-- published on Hackage at the version the plan pins, so withholding
-- the link there was simply wrong.  A boot library from an unreleased
-- GHC can 404, which is rare and honest.
--
-- Local, source-repo and tarball packages still get nothing: for those
-- the version does not identify a Hackage listing, and a link would
-- send the user to a 404 or, worse, to someone else's same-named
-- package.
hackageLink :: PackageName -> Version -> PackageOrigin -> Html ()
hackageLink pkg ver origin = case origin of
  OriginHackage      -> link
  OriginDistribution -> link
  OriginSourceRepo{}    -> mempty
  OriginLocal{}         -> mempty
  OriginLocalTarball{}  -> mempty
  OriginRemoteTarball{} -> mempty
  where
    link = a_ [ class_ "hackage-link"
              , href_ ("https://hackage.haskell.org/package/"
                         <> unPackageName pkg <> "-" <> unVersion ver)
              , target_ "_blank"
              , rel_ "noopener"
              ]
              "\x2197 Hackage"
```

The explicit per-constructor match replaces `hackageLink _ _ _ = mempty`: a new origin then fails to compile here instead of silently losing its link.

- [ ] **Step 4: Run tests, update the golden, commit**

Run: `cabal build all && cabal test all` (accept the `Golden/Server.hs` diff for the containers page gaining a link).

```bash
git add src/Hypha/Server/Ui/Tree.hs src/Hypha/Server/App.hs test/Unit/Server.hs test/Golden/golden
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "fix(server): link boot libraries to Hackage too"
```

---

### Task 16: Instrumentation — parse failures and index format in `doctor`

**Files:**
- Modify: `src/Hypha/Search/Index.hs` (count failures per package into the cache's `kv`)
- Modify: `src/Hypha/Command/Doctor.hs`
- Modify: `test/Unit/Doctor.hs`, `test/Golden/golden` (doctor output)

**Interfaces:**
- Consumes: `ComponentIndex` (Task 9), `readCachedBlob`/`writeCachedBlob`.
- Produces:
  ```haskell
  indexHealthKey :: Text                 -- ^ "index_health"
  data IndexHealth = IndexHealth
    { ihFormat        :: !Int
    , ihParseFailures :: !Int
    , ihNameMismatch  :: !Int
    , ihCabalFallback :: !Int
    }
  ```
  Stored as a small JSON blob under `indexHealthKey`. This is our *own*
  serialisation, so build the `Value` and store it; never decode a blob
  we wrote in the same process.

- [ ] **Step 1: Write the failing test**

In `test/Unit/Doctor.hs` (it already has `extractChecks` and the temp-dir pattern — reuse both):

```haskell
      , testCase "index health is reported when the cache carries it" $
          withSystemTempDirectory "hypha-doctor" $ \tempDir -> do
            cache <- openPackageCacheAt (tempDir </> "global.db") Nothing
            writeCachedBlob cache indexHealthKey $ encodeIndexHealth IndexHealth
              { ihFormat        = currentIndexFormat
              , ihParseFailures = 3
              , ihNameMismatch  = 1
              , ihCabalFallback = 2
              }
            outcome <- runDoctorWith cache
            let checks = extractChecks (outcomeResult outcome)
            assertBool "index section present"
              (KM.member (Key.fromText "index") (asObject checks))
            assertBool "parse failures surfaced"
              ("3" `Text.isInfixOf` renderChecks checks)

      , testCase "an unbuilt index says so rather than reporting zeroes" $
          withSystemTempDirectory "hypha-doctor" $ \tempDir -> do
            cache   <- openPackageCacheAt (tempDir </> "global.db") Nothing
            outcome <- runDoctorWith cache
            let checks = renderChecks (extractChecks (outcomeResult outcome))
            assertBool "explicitly not built"
              (any (`Text.isInfixOf` Text.toLower checks)
                   ["not built", "no index"])
```

`renderChecks` is `Text.decodeUtf8 . BL.toStrict . Aeson.encode`, and `asObject` unwraps the `Value` the existing helpers already return. Zeroes-for-absent is precisely the lie this second case exists to prevent: "no parse failures" and "we never indexed anything" must not render identically.

`runDoctor` currently takes no arguments; add `runDoctorWith :: HyphaPackageCache -> IO (Outcome Value)` and define `runDoctor = openPackageCache Nothing >>= runDoctorWith` so the test can inject a cache without touching `$XDG_CACHE_HOME`.

- [ ] **Step 2: Run it, verify it fails**

Run: `cabal test all --test-options='-p Unit.Doctor'`
Expected: FAIL — no index section.

- [ ] **Step 3: Implement**

```haskell
-- | What the last full index pass ran into.  Written once per pass, so a
-- half-finished pass never overwrites the previous verdict.
data IndexHealth = IndexHealth
  { ihFormat        :: !Int
  , ihParseFailures :: !Int
  , ihNameMismatch  :: !Int
  , ihCabalFallback :: !Int
    -- ^ Components whose module list came from a source-dir walk because
    -- their cabal file could not be parsed.
  }
  deriving stock (Show, Eq)

indexHealthKey :: Text
indexHealthKey = "index_health"

-- | Serialise for the @kv@ blob.  Built structurally and rendered once;
-- nothing in hypha ever decodes this back, so there is no round-trip to
-- get wrong.
encodeIndexHealth :: IndexHealth -> Text
encodeIndexHealth h = Text.decodeUtf8 (BL.toStrict (Aeson.encode (indexHealthValue h)))

indexHealthValue :: IndexHealth -> Value
indexHealthValue h = Aeson.object
  [ "format"         .= ihFormat h
  , "parse_failures" .= ihParseFailures h
  , "name_mismatch"  .= ihNameMismatch h
  , "cabal_fallback" .= ihCabalFallback h
  ]
```

`buildAndCacheIndex` accumulates the three counters across components and writes the blob once at the end of a full pass. `doctor` reads it with `readCachedBlob` and reports; an absent blob is reported as "index not built yet" — a distinct message, never zeroes, because "nothing went wrong" and "nothing was attempted" are different facts.

Doctor is the one place that *does* need to read this blob back. Parse it with `Aeson.decodeStrict` on the value read from the cache and, on a decode failure, report the blob as unreadable rather than substituting zeroes. This is not a self-round-trip: the value crossed a process boundary and a version boundary, so decoding is honest work, not a manufactured failure mode.

- [ ] **Step 4: Take the measurement §1.4 asks for**

Run:
```bash
rm -f ~/.cache/hypha/hypha.db*     # or let the format guard do it
cabal run hypha -- server --port 4287   # let the index finish
cabal run hypha -- doctor
```
Record `ihParseFailures` in the plan's Notes section below. If it is zero, Task 17 is not needed and the spec's §7 stays unimplemented. If non-zero, capture which modules failed from the stderr warnings before deciding.

- [ ] **Step 5: Commit**

```bash
git add src/Hypha/Search/Index.hs src/Hypha/Command/Doctor.hs test/Unit/Doctor.hs test/Golden/golden
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "feat(doctor): report index format and parse-failure counts"
```

---

### Task 17 (conditional on Task 16's measurement): CPP include dirs and `MIN_VERSION_*`

Implement **only** if Task 16 reports a non-zero parse-failure count.

**Files:**
- Create: `src/Hypha/Source/Cpp.hs`
- Modify: `src/Hypha/Source/Parser.hs` (take a `CppEnv`; drop `unsafePerformIO`)
- Modify: `src/Hypha/Project/Components.hs` (`ciIncludeDirs`)
- Create: `test/Unit/SourceCpp.hs`

**Interfaces:**
- Produces:
  ```haskell
  data CppEnv = CppEnv
    { ceIncludeDirs :: ![FilePath]
    , ceDefines     :: ![(Text, Text)]
    }
  minVersionDefines :: BuildPlan -> PackageId -> [(Text, Text)]
  ```

- [ ] **Step 1: Write the failing test**

A fixture module that `#include`s a header from a sibling `include/` dir and guards a declaration behind `#if MIN_VERSION_base(4,0,0)`; assert the guarded declaration appears in the parsed decls, and that it does *not* when `ceDefines` is empty (so the test proves the defines are load-bearing).

- [ ] **Step 2: Run it, verify it fails**

Run: `cabal test all --test-options='-p Unit.SourceCpp'`

- [ ] **Step 3: Implement**

`cpphs` runs with `Cpphs.includes = ceIncludeDirs` and `Cpphs.defines` seeded from `minVersionDefines`, which reads the plan's resolved dependency versions — the same source cabal uses to generate those macros. `parseModuleDocIO` already lives in `IO`; thread `CppEnv` through and delete the `unsafePerformIO` wrappers, replacing the pure `parseDecls`/`parseModuleDoc` with their `IO` counterparts at every call site (`grep -rn 'parseDecls\|parseModuleDoc' src`).

- [ ] **Step 4: Re-measure**

Run `doctor` again and record the new failure count.

- [ ] **Step 5: Commit**

```bash
git add src/Hypha/Source/Cpp.hs src/Hypha/Source/Parser.hs src/Hypha/Project/Components.hs \
        test/Unit/SourceCpp.hs hypha.cabal test/Main.hs
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "fix(source): resolve CPP includes and MIN_VERSION macros from the plan"
```

---

### Task 18: Real-cache regression sweep

**Files:**
- Create: `scripts/index-audit.sh`

**Interfaces:**
- Consumes: a freshly rebuilt `~/.cache/hypha/hypha.db`.

- [ ] **Step 1: Write the audit script**

Create `scripts/index-audit.sh`:

```bash
#!/usr/bin/env bash
# Audit a built hypha index for the row shapes this branch set out to
# kill.  Every query must return 0.
set -euo pipefail
DB="${1:-$HOME/.cache/hypha/hypha.db}"

fail=0
check() {
  local label=$1 sql=$2 n
  n=$(sqlite3 "$DB" "$sql")
  printf '%-44s %s\n' "$label" "$n"
  [ "$n" = "0" ] || fail=1
}

check "module names that are entirely lowercase" \
  "SELECT count(*) FROM pkg_index WHERE mod = lower(mod);"
check "module names containing a source-dir segment" \
  "SELECT count(*) FROM pkg_index WHERE mod LIKE 'src.%' OR mod LIKE 'compiler.%' \
   OR mod LIKE 'lib.%' OR mod LIKE 'test%.%' OR mod LIKE 'src-%';"
check "rows with an empty definition module" \
  "SELECT count(*) FROM pkg_index WHERE def_mod = '';"
check "rows with an unrecognised visibility" \
  "SELECT count(*) FROM pkg_index WHERE visibility NOT IN ('Exposed','Internal');"
check "IntMap symbols carrying a Map signature" \
  "SELECT count(*) FROM pkg_index WHERE mod LIKE 'Data.IntMap%' \
   AND sig LIKE '%Map k a%' AND sig NOT LIKE '%IntMap%';"

exit $fail
```

`chmod +x scripts/index-audit.sh`.

- [ ] **Step 2: Rebuild the index and run the audit**

Run:
```bash
cabal run hypha -- server --port 4287   # wait for indexing to finish, then stop it
./scripts/index-audit.sh
```
Expected: every counter 0, exit 0. Any non-zero counter is a defect this branch was supposed to fix — report the exact query and count rather than adjusting the script.

- [ ] **Step 3: Verify the reported symptoms by hand**

Check each of the six reported items in a browser (the human runs this; the sandbox blocks localhost from the agent session):

1. `/pkg/containers` shows a Hackage link.
2. Searching `containers` puts the package first.
3. Searching `insertWith` shows `Data.Map.Strict`, correctly cased.
4. That result is the wrapper, with a `+N` chip to the internal ones.
5. `/pkg/containers/Data.Map.Strict` has a populated "On this page".
6. `/pkg/containers/Data.Map.Internal` renders instead of erroring.

- [ ] **Step 4: Commit**

```bash
git add scripts/index-audit.sh
git -c user.name='Alfredo Di Napoli' -c user.email='alfredo@well-typed.com' \
  commit -m "test: audit script for the index row shapes this branch fixes"
```

---

## Notes (filled in during execution)

- Task 16 measurement — `ihParseFailures` after §1: _record here_
- Task 17 implemented? _yes/no, with the reason_
- Golden files refreshed: _list_
