{-# Language DataKinds, TypeOperators, OverloadedStrings #-}

module Scrabble.Servant.API where

import           Prelude hiding (Word)
import           Control.Monad.Reader
import qualified Network.HTTP.Simple as Net
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Set as S
import qualified Data.ByteString as BS
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

readDictionary :: BS.ByteString -> Either T.Text Dictionary
readDictionary bs =
    fmap S.fromList . sequence . fmap word . drop 2 . T.lines $ decodeUtf8 bs

getDictionary :: IO Dictionary
getDictionary = do
    putStrLn "starting request for word list"
    res <- Net.httpBS "https://www.wordgamedictionary.com/twl06/download/twl06.txt" 
    putStrLn "finished request for word list"
    when (Net.getResponseStatusCode res /= 200) $
        error "Bad response from URL where word list should be"
    putStrLn "beginning to parse word list"
    case readDictionary (Net.getResponseBody res) of
      Left  msg  -> error $ "Bad word - " ++ T.unpack msg
      Right dict -> do
          putStrLn "finished parsing word list"
          return dict

scrabbleServant :: IO ()
scrabbleServant = do
    dict <- getDictionary
    Warp.run 8081 $ app dict

