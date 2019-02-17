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
newtype Points = Points Int  deriving (Generic, Num)

-- Tiles and Words should always be lowercase, so as to simplify equality

instance Show Points where show (Points i) = show i

----------
-- Conversions to and from text

tiles :: Text -> Either Text Tiles
tiles = fmap Tiles . validText

nTiles :: Tiles -> Int
nTiles (Tiles t) = T.length t

word :: Text -> Either Text Word
word = fmap Word . validText

validText :: Text -> Either Text Text
validText t | T.all C.isAlpha t = return     $ T.toLower t
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
                  -- 12345678910
              'a' -> 1
              'b' ->   3
              'c' ->   3
              'd' ->  2
              'e' -> 1
              'f' ->    4
              'g' ->  2
              'h' ->    4
              'i' -> 1
              'j' ->        8
              'k' ->     5
              'l' -> 1
              'm' ->   3
              'n' -> 1
              'o' -> 1
              'p' ->   3
              'q' ->          10
              'r' -> 1
              's' -> 1
              't' -> 1
              'u' -> 1
              'v' ->    4
              'w' ->    4
              'x' ->        8
              'y' ->    4
              'z' ->          10

              _ -> error "Bad programmer! You wrote a partial function!"

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
    , "f"
    , "g"
    , "h"
    , "i"
    , "j"
    , "k"
    , "l"
    , "m"
    , "n"
    , "o"
    , "p"
    , "q"
    , "r"
    ]
