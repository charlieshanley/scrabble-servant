{-# Language DataKinds, TypeOperators, OverloadedStrings #-}

module Scrabble.Servant.API where

import           Prelude hiding (Word, readFile)
import           Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Set as S
import           Data.Text.IO (readFile)
import           Servant
import           Servant.HTML.Lucid
import qualified Network.Wai.Handler.Warp as Warp
import           Scrabble.Tiles
import           Scrabble.Results

type API = Capture "tiles" Tiles :> Get '[JSON, HTML] Results

type AppM = ReaderT Dictionary Handler

server :: Tiles -> AppM Results
server ts | nTiles ts > 7 = throwError err400 { errBody = "more than 7 tiles" }
server ts                 = results ts

api :: Proxy API
api = Proxy

app :: Dictionary -> Application
app dict = serve api $ hoistServer api (flip runReaderT dict) server

readDictionary :: IO Dictionary
readDictionary = S.fromList . fmap unsafeWord . T.lines <$> readFile "wordlist.txt"

scrabbleServant :: IO ()
scrabbleServant = do
    dictionary <- readDictionary
    Warp.run 8081 $ app dictionary

