{-# Language DataKinds         #-}
{-# Language TypeOperators     #-}
{-# Language OverloadedStrings #-}
{-# Language FlexibleContexts  #-}

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

type GETResults r = Capture "tiles" Tiles :> Get '[JSON, HTML] r

type API = "canmake" :> GETResults CanMake

type AppM = ReaderT Dictionary Handler

server :: ServerT API AppM
server = serveCanMake
    where
        serveCanMake ts | nTiles ts > 8 = err400body "more than 8 tiles"
        serveCanMake ts                 = canMake ts

        err400body body = throwError err400 { errBody = body }

api :: Proxy API
api = Proxy

app :: Dictionary -> Application
app dict = serve api $ hoistServer api (flip runReaderT dict) server

readDictionary :: IO Dictionary
readDictionary = S.fromList . fmap unsafeWord . T.lines <$> readFile "wordlist.txt"

scrabbleServant :: IO ()
scrabbleServant = do
    dictionary <- readDictionary
    Warp.run 80 $ app dictionary

