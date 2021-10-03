{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StatReport where

import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy, toList)
import Data.List (sort)
import Data.Time (diffDays)
import Fmt
import Colonnade

import QuoteData

decimalPlacesFloating :: Int
decimalPlacesFloating = 2

data StatValue = StatValue {
    decimalPlaces :: Int,
    value :: Double
  }

data StatEntry = StatEntry {
    qfield :: QField,
    meanVal :: StatValue,
    medianVal :: StatValue,
    minVal :: StatValue,
    maxVal :: StatValue,
    daysBetweenMinMax :: Int
  }

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

median :: (Fractional a, Ord a, Foldable t) => t a -> a
median xs =
  if odd len
    then medianOdd
    else medianEven
  where
    len = length xs
    mid = len `div` 2
    sorted = sort $ toList xs
    medianOdd = head $ drop mid sorted
    medianEven = 
      let
        mids = take 2 $ drop (mid - 1) sorted
      in sum mids / 2


computeMinMaxDays :: (Ord a, Foldable t) =>
                     (QuoteData -> a) -> t QuoteData -> (a, a, Int)
computeMinMaxDays get quotes = (get minQ, get maxQ, days)
  where
    cmp = comparing get
    minQ = minimumBy cmp quotes
    maxQ = maximumBy cmp quotes
    days = fromIntegral $ abs $ diffDays (day minQ) (day maxQ)

statInfo :: (Functor t, Foldable t) => t QuoteData -> [StatEntry]
statInfo quotes = fmap qFieldStatInfo [minBound .. maxBound]
  where
    decimalPlacesByQField Volume = 0
    decimalPlacesByQField _ = decimalPlacesFloating

    qFieldStatInfo qfield =
      let
        get = field2fun qfield
        (mn, mx, daysBetweenMinMax) =
              computeMinMaxDays get quotes
        decPlaces = decimalPlacesByQField qfield
        meanVal = StatValue decimalPlacesFloating
                            (mean $ fmap get quotes)
        medianVal = StatValue decPlaces (median $ fmap get quotes)
        minVal = StatValue decPlaces mn
        maxVal = StatValue decPlaces mx
      in StatEntry {..}

instance Buildable StatValue where
  build sv = fixedF (decimalPlaces sv) (value sv)

instance Buildable StatEntry where
  build StatEntry {..} =
           ""+||qfield||+": "
             +|meanVal|+" (mean), "
             +|medianVal|+" (median), "
             +|minVal|+" (min), "
             +|maxVal|+" (max), "
             +|daysBetweenMinMax|+" (days)"

textReport :: [StatEntry] -> String
textReport = ascii colStats
  where
    colStats = mconcat
      [ headed "Quote Field" (show . qfield)
      , headed "Mean" (pretty . meanVal)
      , headed "Median" (pretty . medianVal)
      , headed "Min" (pretty . minVal)
      , headed "Max" (pretty . maxVal)
      , headed "Days between Min/Max" (pretty . daysBetweenMinMax)
      ]

showPrice :: Double -> Builder
showPrice = fixedF decimalPlacesFloating
