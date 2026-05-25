{-# LANGUAGE DerivingStrategies #-}
module Hypha.Output.Outcome
  ( Outcome (..)
  , Related (..)
  , Action (..)
  , successOutcome
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

-- | Shape of a /successful/ hypha command envelope.
--
-- Failure has no representation here: a failed command is carried as
-- the 'Left' of an @'Either' 'Hypha.Error.HyphaError' ('Outcome' a)@,
-- and the wire-format failure envelope is built directly from the
-- 'HyphaError'.  Concentrating failure semantics in one type avoids
-- the parallel-error-encoding smell that motivated this design.
data Outcome a = Outcome
  { outcomeResult       :: !a
  , outcomeOutsidePlan  :: !Bool
  , outcomeOverrides    :: ![Text]
  , outcomeActions      :: !(Map Text Text)
  , outcomeRelated      :: ![Related]
  }
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

-- | Bare success outcome with no overrides, actions, or related links.
successOutcome :: a -> Outcome a
successOutcome a = Outcome a False [] mempty []

-- | Update the @outside_plan@ flag on a success outcome.
tagOutsidePlan :: Outcome a -> Bool -> Outcome a
tagOutsidePlan oc flag = oc { outcomeOutsidePlan = flag }
