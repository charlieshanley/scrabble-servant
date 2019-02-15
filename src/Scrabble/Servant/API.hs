{-# Language DataKinds, TypeOperators, OverloadedStrings #-}

module Scrabble.Servant.API where

import           Prelude hiding (Word)
import           Data.Set (Set)
import           Servant
import           Servant.HTML.Lucid
import qualified Network.Wai.Handler.Warp as Warp
import           Scrabble.Tiles
import           Scrabble.Results

type API = Capture "tiles" Tiles :> Get '[JSON, HTML] [Result]

-- TODO refactor with Reader monad? rather than passing Set Word (the dictionary)

server :: Set Word -> Tiles -> Handler [Result]
server _    ts | nTiles ts > 7 = throwError err400 { errBody = "more than 7 tiles" }
server dict ts                 = return $ results dict ts

api :: Proxy API
api = Proxy

app :: Set Word -> Application
app dict = serve api (server dict)

scrabbleServant :: IO ()
scrabbleServant = do
    dictionary <- undefined
    Warp.run 8081 $ app dictionary

