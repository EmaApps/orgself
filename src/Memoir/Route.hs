{-# LANGUAGE TypeApplications #-}

module Memoir.Route where

import Data.Time (Day)
import Ema.Route (IsRoute (..), Slug (unSlug))
import qualified Memoir.Data as Data

data Route
  = Index
  | OnDay Day
  deriving (Show)

instance IsRoute Route where
  toSlug = \case
    Index -> mempty
    OnDay day -> one $ show day
  fromSlug = \case
    [] -> Just Index
    [s] -> OnDay <$> Data.parseDay (toString $ unSlug s)
    _ -> Nothing
