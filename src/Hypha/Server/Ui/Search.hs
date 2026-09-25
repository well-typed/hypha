{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Search
  ( searchInput
  , resultsFragment
  , emptyResults
  , buildingFragment
  , queryErrorFragment
  , highlightTokens
  ) where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid
import Lucid.Base (makeAttributes)

import Hypha.Search.Collapse
  ( Presentation (..), SearchResult (..), SymbolResult (..)
  , definitionPresentation, presentationHref, presentationLabel
  , resultHref )
import Hypha.Search.Query (QueryError (..), scopeToken)
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))

-- | Search input bar with HTMX live-search attributes and an inline
-- progress spinner controlled by the @htmx-request@ class.
--
-- The bar also carries the scope toggle: a real button, so it is
-- reachable with Tab and operable with Enter or Space, and it only ever
-- changes when the user flips it.  Inside a package it offers that
-- package, off by default; elsewhere it starts hidden.  Typing a
-- @pkg:\<name\>@ token followed by a space moves the token into the
-- toggle, on any page.  keybindings.js drives both and mirrors the
-- toggle into the hidden @pkg@ input htmx sends along.
searchInput :: Maybe ComponentKey -> Html ()
searchInput scope = do
  div_ [class_ "search-wrap"] $ do
    span_ [class_ "search-indicator", title_ "Searching\x2026"] $ do
      span_ [class_ "spinner"]     (pure ())
      span_ [class_ "indicator-label"] "Searching\x2026"
    input_ [ type_ "hidden", name_ "pkg", class_ "search-scope-value" ]
    scopeToggle scope
    input_
      [ class_       "search-input"
      , type_        "search"
      , name_        "q"
      , placeholder_ "Search packages, modules, symbols\x2026 (pkg:name to scope)"
      , autocomplete_ "off"
      , autofocus_
      , makeAttributes "hx-get"       "/search"
        -- scope-changed is dispatched by keybindings.js when the toggle
        -- flips, so the results follow it without another keystroke.
      , makeAttributes "hx-trigger"   "keyup changed delay:120ms, scope-changed"
      , makeAttributes "hx-target"    "#results"
      , makeAttributes "hx-swap"      "outerHTML"
        -- Server returns a full <ul id="results"> fragment, so we must
        -- replace the element itself.  The default innerHTML swap would
        -- nest a second <ul.results> inside the existing one, and the
        -- absolute-positioned dropdown styling would shrink to its
        -- (now-tiny) parent.
      , makeAttributes "hx-indicator" ".search-indicator"
        -- The scope hidden input isn't inside a <form>, so htmx won't
        -- pick it up automatically — hx-include names it explicitly.
      , makeAttributes "hx-include"   ".search-scope-value"
      ]
    -- Results render as a floating dropdown anchored to the input, so
    -- search works the same on every page — including the symbol and
    -- source views where the main pane is already filled with content.
    ul_ [class_ "results", id_ "results"] (pure ())

-- | The scope toggle.  @data-page-scope@ remembers the package the page
-- is inside, so switching off a scope captured from a @pkg:@ token can
-- fall back to offering it; without one the toggle hides again.
scopeToggle :: Maybe ComponentKey -> Html ()
scopeToggle scope =
  button_ ([ type_ "button"
           , class_ "search-scope"
           , makeAttributes "aria-pressed" "false"
           ] <> offer) $ do
    span_ [class_ "scope-in"] "in "
    span_ [class_ "scope-name"] (foldMap (toHtml . unComponentKey) scope)
  where
    offer = case scope of
      Just (ComponentKey key) ->
        [ data_ "scope" key
        , data_ "page-scope" key
        , title_ ("Search only in " <> key)
        ]
      Nothing -> [hidden_ ""]

-- | A query that cannot run, said as a row of the results dropdown.
queryErrorFragment :: QueryError -> Html ()
queryErrorFragment err =
  ul_ [class_ "results", id_ "results"] $
    li_ [class_ "empty query-error"] $ case err of
      ScopeMissingName -> do
        "Give "; code_ "pkg:"; " a package name, e.g. "
        code_ (toHtml (scopeToken (ComponentKey "aeson"))); "."
      ConflictingScopes a b -> do
        "Pick one scope: "; code_ (toHtml (scopeToken a)); " or "
        code_ (toHtml (scopeToken b)); "."
      UnknownScope (ComponentKey k) -> do
        "No package "; code_ (toHtml k); " in this build plan."

-- | Render search results as an unordered list.
--
-- The three result kinds render differently because they /are/ different:
-- a package row shows its pinned version, a module row its component, and
-- a symbol row its signature plus, when other presentations of the same
-- definition were folded in, a @+N@ affordance linking the definition
-- site.  An empty hit list still produces a visible \"No matches.\" row —
-- use 'emptyResults' for the truly-empty case (no query in flight).
resultsFragment :: [Text] -> [SearchResult] -> Html ()
resultsFragment tokens results = ul_ [class_ "results", id_ "results"] $
  if null results
    then li_ [class_ "empty"] "No matches."
    else mapM_ entry results
  where
    entry :: SearchResult -> Html ()
    entry r = li_ $ do
      a_ [href_ (resultHref r)] (body r)
      case r of
        -- Gated on 'altRows', not on 'srAlternates': the append clause in
        -- 'altRows' fires exactly when the definition module is no
        -- presentation at all, which is the re-export case and the reason
        -- that clause exists.  Gating on 'srAlternates' hid the disclosure
        -- for precisely those groups, leaving the definition site
        -- unreachable from the result that folded it in.
        ResultSymbol s | not (null (altRows s)) -> alternates s
        _                                      -> mempty

    body :: SearchResult -> Html ()
    body = \case
      ResultPackage pkg ver -> do
        span_ [class_ "name"] (highlightTokens tokens (unPackageName pkg))
        span_ [class_ "sig"]  (toHtml ("package" :: Text))
        span_ [class_ "pkgmod"] (toHtml (unVersion ver))
      ResultModule comp modPath _ -> do
        span_ [class_ "name"] (highlightTokens tokens (unModulePath modPath))
        span_ [class_ "sig"]  (toHtml ("module" :: Text))
        span_ [class_ "pkgmod"] (toHtml (unComponentKey comp))
      ResultSymbol s -> do
        span_ [class_ "name"] (highlightTokens tokens (unSymbolName (srName s)))
        span_ [class_ "sig"]  (toHtml (unSignature (srSignature s)))
        span_ [class_ "pkgmod"]
          (toHtml (unComponentKey (srComponent s) <> " \183 "
                     <> unModulePath (srModule s)))

    -- Nothing is hidden by collapse, and the affordance has to prove it.
    -- A bare count could while every alternate was a module of the same
    -- package; once a group spans packages, "+1" on a base result that
    -- folded in ghc-internal tells the reader nothing they wanted to know.
    -- So every folded-in presentation is named, and each is a link.
    alternates :: SymbolResult -> Html ()
    alternates s = details_ [class_ "alt-group"] $ do
      summary_ [ class_ "alt-count"
               , title_ (altSummary s)
               ]
               (toHtml ("+" <> tshow (length (altRows s))))
      ul_ [class_ "alt-list"] $ mapM_ (altItem s) (altRows s)

    -- One row per module the reader might have expected to find this
    -- under, with the defining one tagged rather than repeated: the
    -- definition module is usually a presentation as well, so emitting a
    -- separate row for it listed it twice and opened N+1 rows behind
    -- "+N".  It is appended only when it is neither the presentation the
    -- reader landed on nor one of the folded-in ones -- which is the
    -- re-export case, where the defining module is not indexed as a
    -- presentation at all.
    altRows :: SymbolResult -> [(Presentation, Bool)]
    altRows s =
      [ (p, p == defined) | p <- srAlternates s ]
        ++ [ (defined, True) | defined /= landed, defined `notElem` srAlternates s ]
      where
        defined = definitionPresentation (srDefinition s)
        landed  = Presentation (srComponent s) (srModule s)

    altItem :: SymbolResult -> (Presentation, Bool) -> Html ()
    altItem s (p, defines) = li_ $ do
      a_ [href_ (presentationHref (srName s) p)]
         (toHtml (presentationLabel p))
      if defines then span_ [class_ "alt-tag"] "defines it" else mempty

    -- The same rows in the tooltip, so hovering answers the question
    -- without opening the list -- and each module named once, which the
    -- previous "also exposed by X; defined in X" could not manage when
    -- the defining module was itself a presentation.
    altSummary s = "also under " <> Text.intercalate ", "
      [ presentationLabel p <> if defines then " (defines it)" else ""
      | (p, defines) <- altRows s
      ]

    tshow :: Int -> Text
    tshow = Text.pack . show

-- | Wrap the first case-insensitive occurrence of every token in
-- @\<mark\>@.  Matches are claimed left-to-right and never overlap or
-- nest; unmatched text passes through verbatim.
highlightTokens :: [Text] -> Text -> Html ()
highlightTokens tokens t = render 0 (claim (sortOn fst candidates) (-1))
  where
    lower = Text.toLower t

    candidates =
      [ (i, Text.length tok)
      | tok0 <- tokens
      , let tok = Text.toLower tok0
      , not (Text.null tok)
      , Just i <- [firstIndex tok]
      ]

    firstIndex needle =
      let (pre, rest) = Text.breakOn needle lower
      in if Text.null rest then Nothing else Just (Text.length pre)

    -- Keep only intervals that start after the previous kept one ends.
    claim [] _ = []
    claim ((s, l) : xs) end
      | s > end   = (s, l) : claim xs (s + l - 1)
      | otherwise = claim xs end

    render pos [] = toHtml (Text.drop pos t)
    render pos ((s, l) : xs) = do
      toHtml (Text.take (s - pos) (Text.drop pos t))
      mark_ (toHtml (Text.take l (Text.drop s t)))
      render (s + l) xs

-- | Empty results UL — used when there is no query in flight so the
-- floating dropdown collapses (CSS @.results:empty { display: none; }@).
emptyResults :: Html ()
emptyResults = ul_ [class_ "results", id_ "results"] (pure ())

-- | Friendly placeholder shown while the in-memory index is still being
-- populated in the background.  The auto re-fetch uses @outerHTML@
-- because the response is itself a full @\<ul id=\"results\"\>@; the
-- default @innerHTML@ swap would nest a fresh @\<ul\>@ inside the
-- existing one on every tick, producing duplicated IDs and broken
-- layout after a handful of refreshes.
buildingFragment :: Html ()
buildingFragment = ul_ [class_ "results", id_ "results"]
  $ li_ [ class_ "building"
        , makeAttributes "hx-get"     "/search"
        , makeAttributes "hx-trigger" "load delay:800ms"
        , makeAttributes "hx-target"  "#results"
        , makeAttributes "hx-swap"    "outerHTML"
        , makeAttributes "hx-include" ".search-input"
        ] $ do
      span_ [class_ "spinner big"] (pure ())
      div_  [class_ "building-text"] $ do
        strong_ "Building the docs\x2026"
        span_ [class_ "building-sub"]
          "Indexing your build plan. Results appear as they come in."
