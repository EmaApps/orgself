{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Memoir.Data.Measure where

import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare.TH
  ( DeriveGCompare (deriveGCompare),
    DeriveGEQ (deriveGEq),
  )
import qualified Data.Map.Strict as Map
import Data.Some
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type Parser a = M.Parsec Void Text a

-- A rating from 1-5.
newtype Rating5 = Rating5 {unRating5 :: Int}
  deriving (Eq, Show, Ord)

ratingSplit :: Rating5 -> (Int, Int)
ratingSplit (Rating5 n) =
  (n, 5 - n)

ratings5P :: Parser Rating5
ratings5P = do
  rating <- digit
  void $ M.char '/'
  void $ M.char '5'
  if rating >= 1 && rating <= 5
    then pure $ Rating5 rating
    else fail "Invalid rating"
  where
    digit = do
      c <- M.digitChar
      maybe (fail "Not num") pure $ readMaybe @Int $ one @String c

data Extent
  = ExtentNone
  | ExtentSome
  | ExtentFull
  deriving (Eq, Show, Ord, Enum, Bounded)

extentP :: Parser Extent
extentP = do
  M.takeRest >>= \case
    "none" -> pure ExtentNone
    "some" -> pure ExtentSome
    "full" -> pure ExtentFull
    x -> fail $ "Invalid value: " <> toString x

data Measure a where
  Measure_Rating5 :: Text -> Measure Rating5
  Measure_Extent :: Text -> Measure Extent

measureName :: Some Measure -> Text
measureName = \case
  Some (Measure_Rating5 s) -> s
  Some (Measure_Extent s) -> s

parseMeasure :: Text -> Text -> DSum Measure Identity
parseMeasure name s = fromMaybe (error $ "Unknown measure: " <> name) $ do
  asum
    [ (Measure_Rating5 name :=>) . Identity <$> M.parseMaybe ratings5P s,
      (Measure_Extent name :=>) . Identity <$> M.parseMaybe extentP s
    ]

lookupMeasure :: Some Measure -> Measures -> Maybe (DSum Measure Identity)
lookupMeasure m =
  find (\x -> m == someMeasure x) . DMap.assocs
  where
    someMeasure :: DSum Measure Identity -> Some Measure
    someMeasure = \case
      x@(Measure_Rating5 _) :=> Identity _ -> Some x
      x@(Measure_Extent _) :=> Identity _ -> Some x

type Measures = DMap Measure Identity

parseMeasures :: Map Text Text -> Measures
parseMeasures =
  DMap.fromList . fmap (uncurry parseMeasure) . Map.toList

deriveGEq ''Measure
deriveGCompare ''Measure
deriveArgDict ''Measure
