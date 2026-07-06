{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Deps
  ( runDeps
  , compactKeys
  , fullKeys
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson (Value, object, (.=))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Hypha.Cli.Types
import Hypha.Output.Outcome  (Outcome (..))
import Hypha.Types.BuildPlan (BuildPlan (..), forwardDepsOf, reverseDepsOf)
import Hypha.Types.PackageId (PackageName (..), Version (..))

compactKeys, fullKeys :: Set.Set Text
compactKeys = Set.fromList ["package", "direction", "depth", "deps"]
fullKeys    = compactKeys

-- | Run the @deps@ command: show forward or reverse dependencies.
runDeps :: Monad m
        => BuildPlan
        -> PackageName
        -> Bool        -- ^ reverse?
        -> Maybe Int   -- ^ depth bound
        -> m (Outcome Value)
runDeps bp name reverseMode mDepth = pure $ Outcome
    (object
      [ "package"   .= unPackageName name
      , "direction" .= (if reverseMode then "reverse" :: Text else "forward")
      , "depth"     .= maybe Aeson.Null Aeson.toJSON mDepth
      , "deps"      .= map encodeDep listing
      ])
    DepsCmd
    False  -- not outside plan
    []     -- no overrides
    (Map.fromList
      [ (unPackageName n, "hypha package " <> unPackageName n)
      | n <- take 5 (map fst listing)
      ])
  where
    listing :: [(PackageName, Version)]
    listing = applyDepth mDepth $
      if reverseMode
        then reverseDepsOf name bp
        else forwardDepsOf name bp

    encodeDep (n, v) = object
      [ "package" .= unPackageName n
      , "version" .= unVersion v
      , "fetch"   .= ("hypha package " <> unPackageName n)
      ]

-- | Apply depth bound to a dependency listing.
applyDepth :: Maybe Int -> [a] -> [a]
applyDepth Nothing  = id
applyDepth (Just n) = take n
