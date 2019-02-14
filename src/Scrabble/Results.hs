{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleInstances #-}

module Scrabble.Results ( Result, results ) where

import           Prelude hiding (Word)
import           Data.Aeson (ToJSON)
import           GHC.Generics
import           Lucid
import           Data.Monoid ((<>))
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
        td_ . toHtml $ wrd r
        td_ . toHtml $ pts r
    toHtmlRaw = toHtml


results :: Tiles -> [Result]
results = undefined
