{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Concurrent.Async      (async)
import           Control.Monad                 (void)
import           Data.ByteString               (ByteString)
import           Data.FileEmbed                (embedFile)
import qualified FoundationDB                  as FDB
import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative.App.Simple (runLoop)

import           FDBE.App                      (app)

styles :: ByteString
styles = $(embedFile "styles.css")

main :: IO ()
main = do
  void $ Gtk.init Nothing
  screen <- maybe (fail "no screen!?") pure =<< Gdk.screenGetDefault
  p <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData p styles
  let priority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER
  Gtk.styleContextAddProviderForScreen screen p priority
  void . async $ do
    FDB.withFoundationDB FDB.defaultOptions $ void . runLoop . app
    Gtk.mainQuit
  Gtk.main
