{-# LANGUAGE OverloadedStrings #-}

module Main where

import FDBE.App (start)
import qualified FoundationDB as FDB

main :: IO ()
main = do
  FDB.withFoundationDB FDB.defaultOptions $ \db ->
    start db
