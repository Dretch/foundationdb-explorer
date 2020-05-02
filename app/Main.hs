{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           FDBE.Prelude

import qualified Control.Concurrent.Async                    as Async
import           Control.Exception                           (finally)
import           Data.FileEmbed                              (embedFile)
import qualified FoundationDB                                as FDB
import qualified GI.Gdk                                      as Gdk
import qualified GI.Gtk                                      as Gtk
import           GI.Gtk.Declarative.App.Simple               (runLoop)
import           GI.Gtk.Declarative.Attributes.Custom.Window (IconData (..),
                                                              setDefaultIcon)

import           FDBE.App                                    (app)

styles :: ByteString
styles = $(embedFile "styles.css")

icon :: IconData
icon = IconDataBytes $(embedFile "icon.png")

main :: IO ()
main = do
  void (Gtk.init Nothing)
  setDefaultIcon icon
  setupStyleSheet
  main' <- Async.async Gtk.main
  FDB.withFoundationDB FDB.defaultOptions (void . runLoop . app)
    `finally` (Gtk.mainQuit >> Async.wait main')

setupStyleSheet :: IO ()
setupStyleSheet = do
  screen <- maybe (fail "no screen!?") pure =<< Gdk.screenGetDefault
  p <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData p styles
  let priority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER
  Gtk.styleContextAddProviderForScreen screen p priority
