{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Deps
  ( runDeps
  , compactKeys
  , fullKeys
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

import Hypha.Output.Outcome  (Outcome (..), Related (..))
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
runDeps bp name reverseMode mDepth = pure $ OutcomeSuccess
    (object
      [ "package"   .= unPackageName name
      , "direction" .= (if reverseMode then "reverse" :: Text else "forward")
      , "depth"     .= maybe Aeson.Null Aeson.toJSON mDepth
      , "deps"      .= map encodeDep listing
      ])
    False  -- not outside plan
    []     -- no overrides
    Map.empty  -- no actions
    [ Related (unPackageName n) ("hypha package " <> unPackageName n)
    | n <- take 5 (map fst listing)
    ]
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
