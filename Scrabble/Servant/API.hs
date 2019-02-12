{-# Language DataKinds         #-}
{-# Language TypeOperators     #-}
{-# Language DeriveGeneric     #-}
{-# Language OverloadedStrings #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts  #-}

module Scrabble.Servant.API where

import           Servant
import           Data.Aeson
import           GHC.Generics
import           Servant.HTML.Lucid
import           Lucid
import           Data.Monoid ((<>))
import qualified Network.Wai.Handler.Warp as Warp
import           Scrabble.Tile

type API = Capture "tiles" [Tile] :> Get '[JSON, HTML] Results


data ScrabbleWord = ScrabbleWord { wrd :: String, pts :: Int }
    deriving Generic

instance ToJSON ScrabbleWord

    
data Results = Results
    { missing0 :: [ScrabbleWord]
    , missing1 :: [ScrabbleWord]
    , missing2 :: [ScrabbleWord]
    } deriving Generic

instance ToJSON Results
instance ToHtml Results where
    toHtml (Results m0 m1 m2) = do
        wordTable m0 "missing 0 letters"
        wordTable m1 "missing 1 letter"
        wordTable m2 "missing 2 letters"
    toHtmlRaw = toHtml

wordTable :: Monad m => [ScrabbleWord] -> HtmlT m () -> HtmlT m ()
wordTable ws capt = table_ $ do
    caption_ capt
    tr_ $ th_ "word" <> th_ "points"
    foldMap toHtml ws

instance ToHtml ScrabbleWord where
    toHtml (ScrabbleWord w p) = tr_ $ do
        td_ . toHtml        $ w
        td_ . toHtml . show $ p
    toHtmlRaw = toHtml



server :: [Tile] -> Handler Results
server (_:_:_:_:_:_:_:_:_) = throwError err400 { errBody = "Seven tiles max." }
server _                 = return res

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

scrabbleServant :: IO ()
scrabbleServant = Warp.run 8081 app

----------
-- For ghci

w1, w2 :: ScrabbleWord
w1 = ScrabbleWord "hello" 12
w2 = ScrabbleWord "goodbye" 17

res :: Results
res = Results [w1, w1, w2] [w2, w2, w1] []

html :: Html ()
html = toHtml res
