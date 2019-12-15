module Main where

import Control.Monad (void)
import GI.Gtk.Declarative.App.Simple (run)

import FoundationDBUtil (getClusterFilePath)
import qualified ClusterFileChooser (State (..), app)
import qualified MainWindow (app)

main :: IO ()
main = do
    path <- getClusterFilePath
    ClusterFileChooser.State clusterFilePath <- run $ ClusterFileChooser.app path
    case clusterFilePath of
        Nothing ->
            putStrLn "No cluster file selected"
        Just clusterFilePath' ->
            void $ run $ MainWindow.app clusterFilePath'
