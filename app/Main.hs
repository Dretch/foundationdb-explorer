module Main where

import Control.Monad (void)
import GI.Gtk.Declarative.App.Simple (run)
import qualified FoundationDB as FDB

import qualified App (app)

main :: IO ()
main = do
    FDB.withFoundationDB FDB.defaultOptions $ \db -> do
        void $ run $ App.app db