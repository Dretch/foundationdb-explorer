module Main where

import Control.Monad (void)
import GI.Gtk.Declarative.App.Simple (run)

import FoundationDBUtil (getClusterFilePath)
import qualified App (app)

main :: IO ()
main = do
    path <- getClusterFilePath
    void $ run $ App.app path