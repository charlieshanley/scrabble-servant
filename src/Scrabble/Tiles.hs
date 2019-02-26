{-# Language FlexibleInstances          #-}
{-# Language FlexibleContexts           #-}
{-# Language InstanceSigs               #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveGeneric              #-}
{-# Language OverloadedStrings          #-}

module Scrabble.Tiles
    ( Tiles, tiles, nTiles, untiles
    , Word, word, unsafeWord
    , Points, score
    , Subsequences, subsequences, permutations
    ) where

import           Prelude hiding (Word, fail)
import           GHC.Generics
import           Data.Char (isAlpha)
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Set (Set)
import qualified Data.Set as S
import           Servant
import           Data.Aeson (ToJSON)
import           Lucid      (ToHtml)


newtype Tiles        = Tiles { untiles :: Text }
newtype Word         = Word   Text deriving (Generic, ToHtml, Eq, Ord)
newtype Points       = Points Int  deriving (Generic, Eq, Ord, Num)
type    Subsequences = [String]

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

-- Does not check if there are non-alpha characters
unsafeWord :: Text -> Word
unsafeWord = Word

validText :: Text -> Either Text Text
validText t | T.all isAlpha t = return     $ T.toLower t
validText t                     = throwError $ t <> " contains nonalpha char"

----------
-- subsequence & permutation helpers for algorithm

subsequences :: Tiles -> Subsequences
subsequences = L.subsequences . T.unpack . untiles

permutations :: Subsequences -> Set Word
permutations  = S.fromList . fmap (Word . T.pack) . (>>= L.permutations)


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
