{-# LANGUAGE DerivingStrategies #-}
module Hypha.Types.Doc
  ( DocText (..)
  ) where

import Data.Text (Text)

-- | Raw Haddock documentation text, before any rendering.
-- This is a newtype to prevent mixing with other 'Text' values.
newtype DocText = DocText { unDocText :: Text }
  deriving stock (Show, Eq)
