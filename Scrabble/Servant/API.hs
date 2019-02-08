{-# Language DataKinds     #-}
{-# Language TypeOperators #-}

module Scrabble.Servant.API where

import Servant
import Scrabble.Tile

type API = Capture "tiles" [Tile] :> Get '[JSON, HTML] Results

newtype ScrabbleWord = ScrabbleWord (String, Int)

data Results = Results
    { missing0 :: [ScrabbleWord]
    , missing1 :: [ScrabbleWord]
    , missing2 :: [ScrabbleWord]
    }


server :: [Tile] -> Handler Results
server (_:_:_:_:_:_:_:_) = throwError "Too many tiles -- 7 is the maximum."
server _                 = undefined
