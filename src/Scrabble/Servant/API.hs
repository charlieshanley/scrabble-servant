{-# Language DataKinds, TypeOperators, OverloadedStrings #-}

module Scrabble.Servant.API where

import           Prelude hiding (Word)
import           Control.Monad.Reader
import           Servant
import           Servant.HTML.Lucid
import qualified Network.Wai.Handler.Warp as Warp
import           Scrabble.Tiles
import           Scrabble.Results

type API = Capture "tiles" Tiles :> Get '[JSON, HTML] [Result]

type AppM = ReaderT Dictionary Handler

server :: Tiles -> AppM [Result]
server ts | nTiles ts > 7 = throwError err400 { errBody = "more than 7 tiles" }
server ts                 = results ts

api :: Proxy API
api = Proxy

app :: Dictionary -> Application
app dict = serve api $ hoistServer api (flip runReaderT dict) server

scrabbleServant :: IO ()
scrabbleServant = do
    let dictionary = testDictionary
    Warp.run 8081 $ app dictionary

