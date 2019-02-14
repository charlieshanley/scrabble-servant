{-# Language DataKinds, TypeOperators, OverloadedStrings #-}

module Scrabble.Servant.API where

import           Servant
import           Servant.HTML.Lucid
import qualified Network.Wai.Handler.Warp as Warp
import           Scrabble.Tiles
import           Scrabble.Results
import           Scrabble.Alg

type API = Capture "tiles" Tiles :> Get '[JSON, HTML] [Result]

server :: Tiles -> Handler [Result]
server ts | nTiles ts > 7 = throwError err400 { errBody = "more than 7 tiles" }
server ts                 = return $ results ts

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

scrabbleServant :: IO ()
scrabbleServant = Warp.run 8081 app

