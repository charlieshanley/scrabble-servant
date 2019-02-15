{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleInstances #-}

module Scrabble.Results ( Result, results ) where

import           Prelude hiding (Word)
import           Data.Aeson (ToJSON)
import           GHC.Generics
import           Lucid
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import           Scrabble.Tiles

data Result = Result { wrd :: Word, pts :: Points }
    deriving Generic

instance ToJSON Result

instance ToHtml [Result] where
    toHtml rs = table_ $ do
        caption_ "You can make:"
        tr_ $ th_ "word" <> th_ "points"
        foldMap toHtml rs
    toHtmlRaw = toHtml

instance ToHtml Result where
    toHtml r = tr_ $ do
        td_ . toHtml        $ wrd r
        td_ . toHtml . show $ pts r
    toHtmlRaw = toHtml


results :: Set Word -> Tiles -> [Result]
results dictionary t = present <$> S.toList myWords
    where myWords = subseqPermutations t `S.intersection` dictionary
          present w = Result w (score w)
