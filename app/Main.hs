{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

-- import           FDBE.Prelude

-- import           Data.FileEmbed                              (embedFile)
import qualified FoundationDB                                as FDB

import           FDBE.App                                    (start)

-- styles :: ByteString
-- styles = $(embedFile "styles.css")

-- icon :: IconData
-- icon = IconDataBytes $(embedFile "icon.png")

main :: IO ()
main = do
  FDB.withFoundationDB FDB.defaultOptions $ \db ->
    start db
    --runWith (setDefaultIcon icon >> setupStyleSheet) App{ database = db, exitEvent = Exit () }

-- setupStyleSheet :: IO ()
-- setupStyleSheet = do
--   screen <- maybe (fail "no screen!?") pure =<< Gdk.screenGetDefault
--   p <- Gtk.cssProviderNew
--   Gtk.cssProviderLoadFromData p styles
--   let priority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER
--   Gtk.styleContextAddProviderForScreen screen p priority
