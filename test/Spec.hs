{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Hspec

import Web.Scotty.Binding.Play

main :: IO ()
main = hspec $ do
    describe "Web.Scotty.Binding.Play" $ do
        return ()
