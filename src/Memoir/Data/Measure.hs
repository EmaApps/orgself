{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Memoir.Data.Measure where

import Data.Dependent.Sum
import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type Parser a = M.Parsec Void Text a

-- A rating from 1-5.
newtype Rating5 = Rating5 {unRating5 :: Int}
  deriving (Eq, Show)

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
  Measure_Rating5 :: Measure Rating5
  Measure_Extent :: Measure Extent
  Measure_Unknown :: Measure Text

parseMeasure :: Text -> DSum Measure Identity
parseMeasure s = fromMaybe (Measure_Unknown :=> Identity s) $ do
  asum
    [ (Measure_Rating5 :=>) . Identity <$> M.parseMaybe ratings5P s,
      (Measure_Extent :=>) . Identity <$> M.parseMaybe extentP s
    ]

type Measures = Map Text (DSum Measure Identity)

parseMeasures :: Map Text Text -> Measures
parseMeasures =
  Map.map parseMeasure
