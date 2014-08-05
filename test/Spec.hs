{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Text.Lazy (Text)
import Network.HTTP.Client (Request)
import qualified Network.HTTP.Client as HTTP
import Test.Hspec
import Test.HUnit
import Web.Scotty

import Web.Scotty.Binding.Play

port :: Int
port = 3000

host :: String
host = "http://localhost:" ++ show port

withScotty :: ScottyM () -> IO a -> IO ()
withScotty app f = bracket
    (forkIO $ scotty port app)
    killThread
    (const $ f >> return ())

type SMethod = RoutePattern -> ActionM () -> ScottyM ()

data Method = GET | POST

toS :: Method -> ActionM () -> ScottyM ()
toS GET  = get "/"
toS POST = post "/"

p :: Method -> [(ByteString, ByteString)] -> Request -> Request
p GET  = HTTP.setQueryString . map (\(k, v) -> (k, Just v))
p POST = HTTP.urlEncodedBody

http :: (Request -> Request) -> IO ()
http f = HTTP.withManager HTTP.defaultManagerSettings $ \mgr -> do
    req <- HTTP.parseUrl $ host ++ "/"
    _ <- HTTP.httpNoBody (f req) mgr
    return ()

data Test1 = Test1
    { field1 :: Int
    , field2 :: Text
    }
  deriving (Show, Eq)

deriveBindable ''Test1

testServer :: (Eq a, Show a, Bindable a)
    => (ActionM () -> b) -> a -> b
testServer m d = m $ do
    t1 <- parseParams "data"
    liftIO $ d @=? t1

test :: (Eq a, Show a, Bindable a)
    => Method -> a -> [(ByteString, ByteString)] -> IO ()
test m ex = withScotty (testServer (toS m) ex) . http . p m

data Test2 = Test2
    { field21 :: [Text]
    }
  deriving (Show, Eq)

deriveBindable ''Test2

data Test3 = Test3
    { field31 :: Test1
    , field32 :: Test2
    }
  deriving (Show, Eq)

deriveBindable ''Test3

main :: IO ()
main = hspec $ do
    describe "Web.Scotty.Binding.Play" $ do
        let ex1 = Test1 1 "test"
        let ac1 =
                [ ("data.field1", "1")
                , ("data.field2", "test")
                ]
        it "bind data GET" $ test GET ex1 ac1
        it "bind data POST" $ test POST ex1 ac1
        let ex2 = 1 :: Int
        let ac2 = [("data", "1")]
        it "bind simple data GET" $ test GET ex2 ac2
        it "bind simple data POST" $ test POST ex2 ac2
        let ex3 = Test2 ["text1", "text2"]
        let ac3 =
                [ ("data.field21[0]", "text1")
                , ("data.field21[1]", "text2")
                ]
        it "bind array data GET" $ test GET ex3 ac3
        it "bind array data POST" $ test POST ex3 ac3
        let ex4 = Test3 ex1 ex3
        let ac4 =
                [ ("data.field31.field1", "1")
                , ("data.field31.field2", "test")
                , ("data.field32.field21[0]", "text1")
                , ("data.field32.field21[1]", "text2")
                ]
        it "bind nested data GET" $ test GET ex4 ac4
        it "bind nested data POST" $ test POST ex4 ac4
