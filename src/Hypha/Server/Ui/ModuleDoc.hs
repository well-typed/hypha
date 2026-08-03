{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The module documentation page.  Renders whichever 'ModuleDocView'
-- the server resolved: embedded prebuilt Haddock, docs extracted from
-- source on the fly, or (last resort) the bare export list with the
-- reason we could not do better.
module Hypha.Server.Ui.ModuleDoc
  ( modulePage
  , anchorFor
  , kindBadge
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

import qualified Hypha.Server.Ui.Haddock as Haddock
import           Hypha.Search.Index (DefinitionRef (..))
import           Hypha.Server.ModuleDoc
import           Hypha.Types.ComponentName (ComponentKey (..))
import           Hypha.Types.Route qualified as Route
import           Hypha.Types.SymbolPath (ModulePath (..))
import           Hypha.Source.Extract
                   (DocEntry (..), EntryOrigin (..), ModuleDocInfo (..))
import           Hypha.Source.Parser (DeclKind (..))
import           Hypha.Types.Doc (DocText (..))

-- | Whole module page body (rendered inside the shell).
modulePage :: Text -> Text -> ModuleDocView -> Html ()
modulePage pkgT modT view = div_ [class_ "mod-doc"] $ do
  moduleHead pkgT modT view
  case view of
    ViewPrebuilt pd    -> prebuiltBody pd
    ViewFromSource sd  -> sourceBody pkgT modT sd
    ViewExportsOnly names reason -> exportsBody pkgT modT names reason

-- | Shared page header: module title, package link, doc-source badge,
-- and the action links.
moduleHead :: Text -> Text -> ModuleDocView -> Html ()
moduleHead pkgT modT view = header_ [class_ "mod-head"] $ do
  h1_ [class_ "mod-title"] (toHtml modT)
  p_ [class_ "meta"] $ do
    toHtml ("in package " :: Text)
    a_ [href_ (Route.hrefFrom ["pkg", pkgT])] (toHtml pkgT)
  div_ [class_ "mod-actions"] $ do
    sourceBadge
    a_ [class_ "action", href_ (Route.hrefFrom ["source", pkgT, modT])]
       "View source"
    rawHaddockAction
  where
    sourceBadge = case view of
      ViewPrebuilt _      -> span_ [class_ "doc-badge badge-prebuilt"]
                               "prebuilt haddock"
      ViewFromSource _    -> span_ [class_ "doc-badge badge-source"]
                               "rendered from source"
      ViewExportsOnly _ _ -> span_ [class_ "doc-badge badge-exports"]
                               "exports only"

    rawHaddockAction = case rawPkgVer of
      Nothing  -> mempty
      Just pv  ->
        a_ [ class_ "action"
           , href_ ("/haddock/" <> pv <> "/"
                     <> Text.replace "." "-" modT <> ".html")
           ]
           "Raw haddock \x2197"

    rawPkgVer = case view of
      ViewPrebuilt pd     -> Just (pdPkgVer pd)
      ViewFromSource sd   -> sdRawHaddock sd
      ViewExportsOnly _ _ -> Nothing

-- | Embedded prebuilt Haddock: description + interface fragments on
-- the left, Haddock's own contents list feeding the rail.
prebuiltBody :: PrebuiltDoc -> Html ()
prebuiltBody pd = div_ [class_ "doc-with-rail"] $ do
  div_ [class_ "doc-body haddock-embed"] $ do
    maybe mempty (div_ [class_ "haddock-description"] . toHtmlRaw)
          (pdDescription pd)
    toHtmlRaw (pdInterface pd)
  case pdContents pd of
    Nothing  -> mempty
    Just toc -> nav_ [class_ "toc-rail"] $ do
      div_ [class_ "toc-title"] "On this page"
      div_ [class_ "toc-haddock"] (toHtmlRaw toc)

-- | Docs rendered on the fly from the module source.
sourceBody :: Text -> Text -> SourceDoc -> Html ()
sourceBody pkgT modT sd = div_ [class_ "doc-with-rail"] $ do
  div_ [class_ "doc-body"] $ do
    case mdiHeader info of
      Nothing -> mempty
      Just (DocText t) ->
        div_ [class_ "haddock module-prose"] (Haddock.renderHaddockHtml t)
    if null (mdiEntries info)
      then p_ [class_ "hint"] "No top-level declarations found."
      else mapM_ (entrySection pkgT modT) (mdiEntries info)
  tocRail (mdiEntries info)
  where
    info = sdInfo sd

-- | One documented declaration.
entrySection :: Text -> Text -> DocEntry -> Html ()
entrySection pkgT modT e =
  section_ [class_ "decl", id_ (anchorFor (deKind e) (deName e))] $ do
    div_ [class_ "decl-head"] $ do
      kindBadge (deKind e)
      a_ [ class_ "decl-name"
         , href_ (Route.hrefFrom ["pkg", pkgT, modT, deName e])
         ]
         (toHtml (deName e))
      a_ [ class_ "decl-anchor"
         , href_ ("#" <> anchorFor (deKind e) (deName e))
         , title_ "Link to this declaration"
         ]
         "#"
      srcLink
      reexportNote
    maybe mempty
          (\sig -> pre_ [class_ "signature"] (code_ (toHtml sig)))
          (deSignature e)
    case deHaddock e of
      Nothing          -> mempty
      Just (DocText t) -> div_ [class_ "haddock"] (Haddock.renderHaddockHtml t)
  where
    -- A wrapper module's entries are documented here but defined
    -- elsewhere.  Saying so, with a link, is the difference between a
    -- page the reader can trust and one that quietly implies the code
    -- lives here.
    reexportNote = case deOrigin e of
      EntryLocal        -> mempty
      -- No link and no claim: we do not know where this is defined, and
      -- the module that came closest is a guess.  Linking it sent the
      -- reader to a page that does not have the symbol.
      EntryUnplaced believed ->
        span_ [ class_ "decl-origin decl-unplaced"
              , title_ ("Re-exported. hypha could not resolve where this is"
                          <> " defined; the nearest candidate import is "
                          <> unModulePath believed)
              ]
              (toHtml ("re-exported, origin unresolved" :: Text))
      EntryReexport def ->
        a_ [ class_ "decl-origin"
           , href_ (Route.hrefFrom [ "pkg", unComponentKey (drComponent def)
                                   , unModulePath (drModule def), deName e ])
           , title_ (if unComponentKey (drComponent def) == pkgT
                       then "Defined in another module of this package"
                       else "Defined in another package")
           ]
           (toHtml ("from " <> originLabel def))

    -- The package is named only when it differs, so an intra-package
    -- re-export reads exactly as it did before.
    originLabel def
      | unComponentKey (drComponent def) == pkgT = unModulePath (drModule def)
      | otherwise =
          unComponentKey (drComponent def) <> ":" <> unModulePath (drModule def)

    -- The source link follows the definition, because that is where the
    -- lines this entry reports actually are — including into another
    -- package.
    -- 'Nothing' for an entry we could not place: there is no module we
    -- can honestly send the reader to.
    srcTarget = case deOrigin e of
      EntryLocal        -> Just (pkgT, modT)
      EntryReexport def -> Just ( unComponentKey (drComponent def)
                                , unModulePath (drModule def) )
      EntryUnplaced _   -> Nothing

    srcLink = case (srcTarget, anchorLine) of
      (Just (srcComponent, srcModule), Just n) ->
        a_ [ class_ "decl-src"
           , href_ (Route.hrefFrom ["source", srcComponent, srcModule]
                     <> "?line=" <> tshow n <> "#L" <> tshow n)
           , title_ "Jump to source"
           ]
           "src"
      _ -> mempty
    anchorLine = case deSigLine e of
      Just n  -> Just n
      Nothing -> deDefLine e

-- | Sticky \"On this page\" rail generated from the entries, grouped
-- into types and values.  Hidden on narrow viewports by CSS.
tocRail :: [DocEntry] -> Html ()
tocRail entries
  | null entries = mempty
  | otherwise = nav_ [class_ "toc-rail"] $ do
      div_ [class_ "toc-title"] "On this page"
      tocGroup "Types"  [ e | e <- entries, kindGroup e == GroupType ]
      tocGroup "Values" [ e | e <- entries, kindGroup e == GroupValue ]
      -- Its own group rather than folded into "Values": we do not know
      -- which namespace these belong to, and guessing is what put every
      -- type in base's Prelude under "Values".
      tocGroup "Unresolved" [ e | e <- entries, kindGroup e == GroupUnknown ]
  where
    tocGroup :: Text -> [DocEntry] -> Html ()
    tocGroup _ [] = mempty
    tocGroup label es = do
      div_ [class_ "toc-group"] (toHtml label)
      ul_ [class_ "toc-list"] $
        mapM_ (\e -> li_ $
                 a_ [href_ ("#" <> anchorFor (deKind e) (deName e))]
                    (toHtml (deName e)))
              es

-- | Last-resort view: names only, with the reason shown — degradation
-- is never silent.
exportsBody :: Text -> Text -> [Text] -> Text -> Html ()
exportsBody pkgT modT names reason = div_ [class_ "doc-body"] $ do
  p_ [class_ "warn"] (toHtml reason)
  if null names
    then p_ [class_ "hint"] "No exports detected."
    else ul_ [class_ "export-list"] $
      mapM_ (\nm -> li_ $
               a_ [href_ (Route.hrefFrom ["pkg", pkgT, modT, nm])]
                  (code_ (toHtml nm)))
            names

-- | Haddock-compatible anchor for a declaration: values get @v:@,
-- types get @t:@ — matching the anchors prebuilt pages use, so
-- @#frag@ links resolve the same whichever view renders the module.
-- An entry we could not place gets the bare name: @v:@ would be a claim
-- about a namespace we do not know, and a wrong one breaks the @t:@ links
-- prebuilt pages emit.
anchorFor :: Maybe DeclKind -> Text -> Text
anchorFor mk nm = case kindNamespace mk of
  GroupType    -> "t:" <> nm
  GroupValue   -> "v:" <> nm
  GroupUnknown -> nm

-- | Which \"On this page\" group an entry belongs to.
data KindGroup = GroupType | GroupValue | GroupUnknown
  deriving Eq

kindGroup :: DocEntry -> KindGroup
kindGroup = kindNamespace . deKind

kindNamespace :: Maybe DeclKind -> KindGroup
kindNamespace = \case
  Nothing -> GroupUnknown
  Just k  -> if isTypeKind k then GroupType else GroupValue

isTypeKind :: DeclKind -> Bool
isTypeKind = \case
  DkData        -> True
  DkNewtype     -> True
  DkClass       -> True
  DkTypeSyn     -> True
  DkTypeFamily  -> True
  DkConstructor -> False
  _             -> False

-- | Small badge naming the declaration form.  Functions carry no badge
-- — they are the common case and the signature already says it all.
-- An entry with no declaration behind it carries no badge either: there
-- is nothing to name.
kindBadge :: Maybe DeclKind -> Html ()
kindBadge = maybe mempty badgeFor
  where
    badgeFor :: DeclKind -> Html ()
    badgeFor = \case
      DkFunction     -> mempty
      DkData         -> badge "kb-type"    "data"
      DkNewtype      -> badge "kb-type"    "newtype"
      DkClass        -> badge "kb-class"   "class"
      DkTypeSyn      -> badge "kb-type"    "type"
      DkTypeFamily   -> badge "kb-type"    "type family"
      DkPatternSyn   -> badge "kb-pattern" "pattern"
      DkForeign      -> badge "kb-foreign" "foreign"
      DkClassMethod  -> badge "kb-method"  "method"
      DkConstructor  -> badge "kb-con"     "constructor"

    badge :: Text -> Text -> Html ()
    badge cls label =
      span_ [class_ ("kind-badge " <> cls)] (toHtml label)

tshow :: Int -> Text
tshow = Text.pack . show
