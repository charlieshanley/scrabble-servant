{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

module Scrabble.Results ( Result, Dictionary, results ) where

import           Prelude hiding (Word)
import           Control.Monad.Reader
import           Data.Aeson (ToJSON)
import           GHC.Generics
import           Lucid
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import           Scrabble.Tiles

data Result = Result { wrd :: Word, pts :: Points }
    deriving Generic

type Dictionary = Set Word
-- Should be lower case

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


-- results :: Tiles -> Reader Dictionary [Result]
results :: MonadReader Dictionary m => Tiles -> m [Result]
results t = do
    dictionary <- ask
    let myWords = subseqPermutations t `S.intersection` dictionary
    let package w = Result w (score w)
    return $ package <$> S.toList myWords
