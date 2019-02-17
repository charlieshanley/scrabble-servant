{-# Language FlexibleInstances          #-}
{-# Language FlexibleContexts           #-}
{-# Language InstanceSigs               #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveGeneric              #-}
{-# Language OverloadedStrings          #-}

module Scrabble.Tiles
    ( Tiles, tiles, nTiles, subseqPermutations
    , Word, word
    , Points, score
    , testDictionary
    ) where

import           Prelude hiding (Word, fail)
import           Control.Monad.Except
import           GHC.Generics
import qualified Data.Char as C
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.List as L
import           Servant
import           Data.Aeson (ToJSON)
import           Lucid      (ToHtml)


newtype Tiles  = Tiles  Text
newtype Word   = Word   Text deriving (Generic, ToHtml, Eq, Ord)
newtype Points = Points Int  deriving (Generic, Show, Num)

----------
-- Conversions to and from text

tiles :: Text -> Either Text Tiles
tiles = fmap Tiles . validText

nTiles :: Tiles -> Int
nTiles (Tiles t) = T.length t

word :: Text -> Either Text Word
word = fmap Word . validText

validText :: Text -> Either Text Text
validText t | T.all C.isAlpha t = return     $ T.toUpper t
validText t                     = throwError $ t <> " contains nonalpha char"


----------
-- utlities for algorithm

subseqPermutations :: Tiles -> Set Word
subseqPermutations (Tiles t) =
    S.fromList $ fmap (Word . T.pack) .
    (>>= L.permutations) . L.subsequences $ T.unpack t 



----------
-- Points

score :: Word -> Points
score (Word t) = T.foldl' add 0 t

    where add :: Points -> Char -> Points
          add accum char = accum + points char

          points :: Char -> Points
          points c = case c of
              'Q' -> 10
              'Z' -> 10

              'J' -> 8
              'X' -> 8

              'K' -> 5

              'F' -> 4
              'H' -> 4
              'V' -> 4
              'W' -> 4
              'Y' -> 4

              'B' -> 3
              'C' -> 3
              'M' -> 3
              'P' -> 3

              'D' -> 2
              'G' -> 2

              _ -> 1

----------
-- Instances for Servant

instance ToJSON Points
instance ToJSON Word

instance FromHttpApiData Tiles where
    parseUrlPiece :: Text -> Either Text Tiles
    parseUrlPiece = tiles

----------
-- temp for test

testDictionary :: Set Word
testDictionary = S.fromList $ Word <$>
    [ "a"
    , "b"
    , "c"
    , "d"
    , "e"
    ]
