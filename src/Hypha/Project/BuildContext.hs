{-# LANGUAGE DerivingStrategies #-}
-- | What the plan says about how a package's sources are to be read.
--
-- Two facts travel together because they are both answers to "how would
-- the compiler have seen this file", and a reader that has one without
-- the other gets the module wrong in a way it cannot detect:
--
--   * the CPP environment its @#if@s are evaluated against, and
--   * the platform its conditional stanzas are resolved for.
--
-- Threading them as one value is also what keeps them from drifting: they
-- are derived once, in "Hypha.Project.Plan", from the plan that decided
-- both.
module Hypha.Project.BuildContext
  ( BuildContext (..)
  , hostBuildContext
  , withIncludeDirs
  ) where

import Distribution.System (Platform, buildPlatform)

import Hypha.Source.CppMacros (CppEnv (..), noCppEnv)

-- | The plan-derived context for reading one package's sources.
data BuildContext = BuildContext
  { bcCpp      :: !CppEnv
    -- ^ Macros and include path handed to the preprocessor.
  , bcPlatform :: !Platform
    -- ^ The platform @os()@ and @arch()@ conditions are resolved for.
    -- The plan's, not the host's, whenever a plan was read: a plan
    -- solved for another platform lists another platform's modules.
  }
  deriving stock (Show, Eq)

-- | No plan: no macros, no include path, and the platform hypha is
-- running on — the best available answer for a source read outside a
-- project, and the right one on the overwhelmingly common path where
-- the plan was solved here.
hostBuildContext :: BuildContext
hostBuildContext = BuildContext
  { bcCpp      = noCppEnv
  , bcPlatform = buildPlatform
  }

-- | Prepend include directories, nearest first.
--
-- A component's own @include-dirs@ must win over the compiler's: a
-- package that ships a header shadowing one of GHC's means the shadow.
withIncludeDirs :: [FilePath] -> BuildContext -> BuildContext
withIncludeDirs dirs ctx = ctx
  { bcCpp = (bcCpp ctx) { cppIncludeDirs = dirs <> cppIncludeDirs (bcCpp ctx) } }
