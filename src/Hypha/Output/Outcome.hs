{-# LANGUAGE DerivingStrategies #-}
module Hypha.Output.Outcome
  ( Outcome (..)
  , OutcomeError (..)
  , Related (..)
  , Action (..)
  , successOutcome
  , failureOutcome
  , outcomeActions
  , outcomeResult
  , outcomeRelated
  , outcomeOutsidePlan
  , outcomeOverrides
  , outcomeError
  , tagOutsidePlan
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)

-- | A single cross-reference link emitted inside the envelope.
data Related = Related
  { relatedLabel :: !Text
  , relatedFetch :: !Text
  }
  deriving stock (Show, Eq)

-- | Named action string that can be fed back into hypha.
data Action = Action
  { actionName  :: !Text
  , actionFetch :: !Text
  }
  deriving stock (Show, Eq)

-- | Error payload embedded in a failure envelope.
data OutcomeError = OutcomeError
  { oeCode     :: !Text
  , oeMessage  :: !Text
  , oeExitCode :: !Int
  }
  deriving stock (Show, Eq)

-- | The polymorphic result of a hypha command.
-- We avoid record syntax on sum-type constructors to eliminate
-- partial selectors (caught by -Wpartial-fields).
data Outcome a
  = OutcomeSuccess
      !a           -- ^ result
      !Bool        -- ^ outside plan
      ![Text]      -- ^ overrides
      !(Map Text Text) -- ^ actions
      ![Related]   -- ^ related
  | OutcomeFailure
      !OutcomeError
      !(Map Text Text) -- ^ actions
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

-- Total accessors

outcomeResult :: Outcome a -> Maybe a
outcomeResult (OutcomeSuccess r _ _ _ _) = Just r
outcomeResult (OutcomeFailure _ _)     = Nothing

outcomeOutsidePlan :: Outcome a -> Bool
outcomeOutsidePlan (OutcomeSuccess _ o _ _ _) = o
outcomeOutsidePlan (OutcomeFailure _ _)     = False

outcomeOverrides :: Outcome a -> [Text]
outcomeOverrides (OutcomeSuccess _ _ o _ _) = o
outcomeOverrides (OutcomeFailure _ _)     = []

outcomeActions :: Outcome a -> Map Text Text
outcomeActions (OutcomeSuccess _ _ _ a _) = a
outcomeActions (OutcomeFailure _ a)     = a

outcomeRelated :: Outcome a -> [Related]
outcomeRelated (OutcomeSuccess _ _ _ _ r) = r
outcomeRelated (OutcomeFailure _ _)     = []

outcomeError :: Outcome a -> Maybe OutcomeError
outcomeError (OutcomeSuccess _ _ _ _ _) = Nothing
outcomeError (OutcomeFailure e _)     = Just e

-- | Convenience constructor for a success outcome.
successOutcome :: a -> Outcome a
successOutcome a = OutcomeSuccess a False [] mempty []

-- | Convenience constructor for a failure outcome.
failureOutcome :: OutcomeError -> Outcome a
failureOutcome err = OutcomeFailure err mempty

-- | Update the @outside_plan@ flag on a success outcome.
tagOutsidePlan :: Outcome a -> Bool -> Outcome a
tagOutsidePlan (OutcomeSuccess r _ o a rel) flag =
  OutcomeSuccess r flag o a rel
tagOutsidePlan (OutcomeFailure err a) _ = OutcomeFailure err a
