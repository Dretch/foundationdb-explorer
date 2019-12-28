module Main where

import           Control.Monad                 (void)
import qualified FoundationDB                  as FDB
import           GI.Gtk.Declarative.App.Simple (run)

import qualified App                           (app)

main :: IO ()
main = do
  FDB.withFoundationDB FDB.defaultOptions $ void . run . App.app
