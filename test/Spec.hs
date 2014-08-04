{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
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

data Test1 = Test1
    { field1 :: Int
    , field2 :: Text
    , field3 :: ()
    }
  deriving (Eq)

deriveBindable ''Test1

testServer :: ScottyM ()
testServer = get "/" $ do
    t1 <- parseParams "data"
    liftIO $ do
        1 @=? field1 t1
        "test" @=? field2 t1
        () @=? field3 t1

main :: IO ()
main = hspec $ do
    describe "Web.Scotty.Binding.Play" $ do
        it "" $ withScotty testServer $
            HTTP.withManager HTTP.defaultManagerSettings $ \mgr -> do
                req <- HTTP.parseUrl $ host ++ "/?data.field1=1&data.field2=test&data.field3"
                HTTP.httpNoBody req mgr
