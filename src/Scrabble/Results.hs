{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

module Scrabble.Results ( Results, Dictionary, results ) where

import           Prelude hiding (Word)
import           Control.Monad.Reader
import           Data.Aeson (ToJSON)
import           GHC.Generics
import           Lucid
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.List as L
import           Scrabble.Tiles

data Results = Results { missing0 :: [Play], missing1 :: [Play] }
    deriving ( Generic )

data Play = Play { wrd :: Word, pts :: Points }
    deriving ( Generic )

type Dictionary   = Set Word -- Should be lower case

instance ToJSON Play
instance ToJSON Results

instance ToHtml Play where
    toHtml (Play w p) = tr_ $ (td_ $ toHtml w) <> (td_ $ toHtml $ show p)
    toHtmlRaw = toHtml

instance ToHtml Results where
    toHtml (Results miss0 miss1) = table0 <> table1
        where table0 = makeTable "You can make:"             miss0
              table1 = makeTable "You are missing one from:" miss1
              makeTable capt plays = table_ $ do
                  _ <- caption_ capt
                  tr_ $ th_ "word" <> th_ "points"
                  foldMap toHtml plays
    toHtmlRaw = toHtml


-- results :: MonadReader Dictionary m => Tiles -> m Results
-- results t = do
--     dictionary <- ask
--     let myWords = S.toList $ subseqPermutations t `S.intersection` dictionary
--     let resList = (\w -> Result w (score w)) <$> myWords
--     let descending (Result _ p1) (Result _ p2) = compare p2 p1
--     return $ L.sortBy descending resList

results :: MonadReader Dictionary m => Tiles -> m Results
results ts = do
    dict <- ask
    let subseqs      = subsequences ts
    let subseqsPlus1 = ['a'..'z'] >>= \c -> (c :) <$> subseqs
    let miss0 = legalPlays dict subseqs
    let miss1 = take 20 $ legalPlays dict subseqsPlus1
    return $ Results miss0 miss1

legalPlays :: Dictionary -> Subsequences -> [Play]
legalPlays dict s = L.sortBy descending plays
    where 
        legalWords = S.intersection (permutations s) dict
        plays      = fmap play . S.toList $ legalWords
        play :: Word -> Play
        play w = Play w (score w)
        descending :: Play -> Play -> Ordering
        descending (Play _ p1) (Play _ p2) = compare p2 p1

