-- | UTF-8 at every text boundary, whatever the locale says.
--
-- GHC derives each 'System.IO.Handle'\'s encoding from the process
-- locale, so under @C@/@POSIX@ non-ASCII text is an 'IOError' rather
-- than a mangled glyph (issue #9).  The locale is the wrong authority
-- for us regardless: Haskell sources, @.cabal@ files, JSON and Haddock
-- HTML are UTF-8 by their own specs.
module Hypha.Encoding
  ( setUtf8Encoding
  ) where

import GHC.IO.Encoding (setFileSystemEncoding, setLocaleEncoding, utf8)
import GHC.IO.Encoding.Failure (CodingFailureMode (RoundtripFailure))
import GHC.IO.Encoding.UTF8 (mkUTF8)
import System.IO (hSetEncoding, stderr, stdout)

-- | Pin UTF-8 for the process.  Call it first thing in @main@, before
-- any output.
--
-- All three settings are needed.  The standard handles already exist
-- when @main@ starts, so the locale encoding cannot retrofit them; and
-- conversely the handle settings do not survive
-- 'GHC.IO.Handle.hDuplicateTo', which 'Hypha.Cli.Run.withQuietIfNotVerbose'
-- uses to restore stdout and stderr — GHC re-derives the codec there
-- from the /locale/ encoding.
setUtf8Encoding :: IO ()
setUtf8Encoding = do
  -- Handles opened later: sources, .cabal files, Haddock pages, and
  -- the pipes we read @haddock@ and @ghc@ through.
  setLocaleEncoding utf8

  -- Arguments, environment and file names.  Roundtripping (PEP383
  -- surrogate escapes) is what GHC itself defaults to here, and we keep
  -- it: a path on disk need not be valid UTF-8, and escaping such bytes
  -- beats throwing on them.  Only the base codec changes, which is the
  -- point — under @C@ the default escapes /every/ byte >= 0x80, so
  -- @hypha lookup café@ would reach us mangled and silently answer a
  -- different question.
  setFileSystemEncoding (mkUTF8 RoundtripFailure)

  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
