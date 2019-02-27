{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Scrabble.Results 
    ( CanMake,       canMake
    , CanAlmostMake, canAlmostMake
    , Dictionary
    ) where

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

newtype CanMake       = CanMake       [Play] deriving (Generic, ToJSON)
newtype CanAlmostMake = CanAlmostMake [Play] deriving (Generic, ToJSON)

data Play = Play { wrd :: Word, pts :: Points } deriving (Generic)
instance ToJSON Play

type Dictionary   = Set Word -- Should be lower case

----------
-- content type instances

instance ToHtml Play where
    toHtml (Play w p) = tr_ $ (td_ $ toHtml w) <> (td_ $ toHtml $ show p)
    toHtmlRaw = toHtml

instance ToHtml CanMake where
    toHtml (CanMake plays) = makeTable "You can make:" plays
    toHtmlRaw = toHtml

instance ToHtml CanAlmostMake where
    toHtml (CanAlmostMake plays) = makeTable "You are missing letter one from:" plays
    toHtmlRaw = toHtml

makeTable :: Monad m => HtmlT m a -> [Play] -> HtmlT m ()
makeTable capt plays = table_ $ do
    _ <- caption_ capt
    tr_ $ th_ "word" <> th_ "points"
    foldMap toHtml plays

----------
-- get results for tiles

canMake :: MonadReader Dictionary m => Tiles -> m CanMake
canMake ts = CanMake . legalPlays (subsequences ts) <$> ask

canAlmostMake :: MonadReader Dictionary m => Tiles -> m CanAlmostMake
canAlmostMake ts = CanAlmostMake . legalPlays subseqsPlusOne <$> ask
    where subseqsPlusOne = ['a'..'z'] >>= \c -> (c:) <$> subsequences ts
              
legalPlays :: Subsequences -> Dictionary -> [Play]
legalPlays s dict = L.sortBy descending plays
    where 
        legalWords = S.intersection (permutations s) dict
        plays = fmap play . S.toList $ legalWords
        descending (Play _ p1) (Play _ p2) = compare p2 p1

play :: Word -> Play
play w = Play w (score w)
