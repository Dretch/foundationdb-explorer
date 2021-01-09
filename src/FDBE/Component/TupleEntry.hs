{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | A component for editing foundationdb tuples
module FDBE.Component.TupleEntry
  ( TupleEntry(..)
  , tupleEntry
  ) where

import           FDBE.Prelude

import           Data.Foldable                 (for_)
import           Data.List.Index               (deleteAt, setAt)
import qualified Data.UUID                     as UUID
import qualified Data.Vector                   as Vector
import qualified FoundationDB.Layer.Tuple      as LT
import           FoundationDB.Versionstamp     (TransactionVersionstamp (..),
                                                Versionstamp (..),
                                                VersionstampCompleteness (..))
import           GI.Gdk                        (EventFocus)
import           GI.Gtk                        (Align (..), Box (..), Button (..),
                                                Entry (..), Label (..),
                                                Orientation (..), entryGetText)
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Components

import           FDBE.Bytes                    (bytesToText, textToBytes)
import           FDBE.FoundationDB             (EditableBytes, EditableElem(..), fromEditableElem, toEditableElem, elemText)
import qualified FDBE.Component.ComboBoxBool   as ComboBoxBool
import qualified FDBE.Component.ComboBoxText   as ComboBoxText
import qualified FDBE.Component.IntegerSpinner as IntegerSpinner
import qualified FDBE.Component.TextView       as TextView
import qualified FDBE.Component.DoubleSpinner  as DoubleSpinner

data TupleEntry event = TupleEntry
  { value :: EditableBytes
  , onChanged :: Maybe (EditableBytes -> event)
  , sensitive :: Bool
  }

-- | The default TupleEntry
tupleEntry :: TupleEntry event
tupleEntry = TupleEntry
  { value = Left ""
  , onChanged = Nothing
  , sensitive = True
  }

type Event = ComponentAction TupleEntry

instance Component TupleEntry where

  data ComponentState TupleEntry = TupleEntryState

  data ComponentAction TupleEntry =  TupleValueChanged EditableBytes

  createComponent _decl =
    (TupleEntryState, Nothing)
  
  patchComponent state _decl =
    state
  
  update TupleEntry{..} = \case
    TupleValueChanged x ->
      for_ onChanged $ \f ->
        updateParent (f x)

  view TupleEntry{..} _state =
    container Box [#spacing := 2, #hexpand := True, #sensitive := sensitive]
      [ BoxChild defaultBoxChildProperties combo
      , BoxChild defaultBoxChildProperties { expand = True, fill = True } entry
      ]
    where
      combo :: Widget (ComponentAction TupleEntry)
      combo = component ComboBoxText.comboBoxText
        { ComboBoxText.rawAttributes = [#valign := AlignStart]
        , ComboBoxText.choices = ["Raw", "Tuple"]
        , ComboBoxText.position = Just comboIndex
        , ComboBoxText.onChanged = Just (TupleValueChanged . onTypeChange)
        }

      comboIndex = either (const 0) (const 1) value

      onTypeChange i =
        case (i, value) of
          (0, Right t) ->
            Left . bytesToText . LT.encodeTupleElems $ fromEditableElem <$> t
          (1, Left t) | Right tup <- LT.decodeTupleElems (textToBytes t) ->
            Right $ toEditableElem <$> tup
          (1, Left _) ->
            Right [SingleLineText ""]
          _ ->
            value

      entry :: Widget Event
      entry =
        case value of
          Left text   -> rawEntry text
          Right elems -> tupleEntry' elems (TupleValueChanged . Right)

rawEntry :: Text -> Widget Event
rawEntry text =
  widget Entry
    [ #text := text
    , #tooltipText := escapeSyntaxHelp
    , #hexpand := True
    , #valign := AlignStart
    , onM #changed (fmap (TupleValueChanged . Left) . entryGetText)
    ]

tupleEntry' :: [EditableElem] -> ([EditableElem] -> Event) -> Widget Event
tupleEntry' elems onChanged =
  container Box [#orientation := OrientationVertical, #spacing := 2]
    $ Vector.imap elemEntry (Vector.fromList elems) `Vector.snoc` addElemButton
  where
    elemEntry :: Int -> EditableElem -> BoxChild Event
    elemEntry i field =
      BoxChild defaultBoxChildProperties $
        container Box [#spacing := 2]
          [ BoxChild defaultBoxChildProperties combo
          , BoxChild defaultBoxChildProperties { expand = True, fill = True } input
          , BoxChild defaultBoxChildProperties removeButton
          ]
      where
        combo :: Widget Event
        combo =
          component ComboBoxText.comboBoxText
            { ComboBoxText.choices =
                [ "None"
                , "Bytes"
                , "Text"
                , "Text (long)"
                , "Int"
                , "Float"
                , "Double"
                , "Bool"
                , "UUID"
                , "Versionstamp"
                , "Tuple"
                ]
            , ComboBoxText.position = Just position
            , ComboBoxText.onChanged = Just (onChanged . onElemTypeChange)
            }

        removeButton :: Widget Event
        removeButton =
          widget Button
            [ #label := "Remove"
            , on #clicked (onChanged (deleteAt i elems))
            ]

        position :: Int
        input :: Widget Event
        (position, input) = case field of
          None             -> (0, noneInput)
          Bytes bs         -> (1, bytesInput bs)
          SingleLineText t -> (2, singleLineTextInput t)
          MultiLineText t  -> (3, multiLineTextInput t)
          Int x            -> (4, intInput $ fromIntegral x)
          Float f          -> (5, floatInput f)
          Double d         -> (6, doubleInput d)
          Bool b           -> (7, boolInput b)
          UUID a b c d     -> (8, uuidInput a b c d)
          CompleteVS vs    -> (9, completeVsInput vs)
          IncompleteVS _   -> (9, completeVsInput minBound)
          Tuple t          -> (10, tupleInput t)

        onElemTypeChange :: Int -> [EditableElem]
        onElemTypeChange typePos = setAt i newField elems
          where
            newField = case typePos of
              0  -> None
              1  -> Bytes ""
              2  -> SingleLineText (fromMaybe "" (elemText field))
              3  -> MultiLineText (fromMaybe "" (elemText field))
              4  -> Int 0
              5  -> Float 0
              6  -> Double 0
              7  -> Bool False
              8  -> UUID 0 0 0 0
              9  -> CompleteVS minBound
              10 -> Tuple []
              _  -> error "invalid tuple element type position - this is a bug"

        noneInput :: Widget Event
        noneInput =
          widget Label []

        bytesInput :: ByteString -> Widget Event
        bytesInput bs =
          widget Entry
            [ #text := bytesToText bs
            , #tooltipText := escapeSyntaxHelp
            , #hexpand := True
            , #valign := AlignStart
            , onM #changed (fmap (replaceElemOnChanged . Bytes . textToBytes) . entryGetText)
            ]

        singleLineTextInput :: Text -> Widget Event
        singleLineTextInput t =
          widget Entry
            [ #text := t
            , #hexpand := True
            , #valign := AlignStart
            , onM #changed (fmap (replaceElemOnChanged . SingleLineText) . entryGetText)
            ]

        multiLineTextInput :: Text -> Widget Event
        multiLineTextInput t =
          component TextView.textView
            { TextView.rawAttributes = [#hexpand := True]
            , TextView.value = t
            , TextView.onChanged = Just (replaceElemOnChanged . MultiLineText)
            }

        boolInput :: Bool -> Widget Event
        boolInput b =
          component $ ComboBoxBool.comboBoxBool b (Just (replaceElemOnChanged . Bool))

        floatInput :: Float -> Widget Event
        floatInput f =
          component DoubleSpinner.doubleSpinner
            { DoubleSpinner.value = realToFrac f
            , DoubleSpinner.onChanged = Just (replaceElemOnChanged . Float . realToFrac)
            }

        doubleInput :: Double -> Widget Event
        doubleInput d =
          component DoubleSpinner.doubleSpinner
            { DoubleSpinner.value = d
            , DoubleSpinner.onChanged = Just (replaceElemOnChanged . Double)
            }

        intInput :: Integer -> Widget Event
        intInput x =
          component IntegerSpinner.integerSpinner
            { IntegerSpinner.value = fromIntegral x :: Int
            , IntegerSpinner.onChanged =  Just (replaceElemOnChanged . Int . fromIntegral)
            }

        uuidInput :: Word32 -> Word32 -> Word32 -> Word32 -> Widget Event
        uuidInput a b c d =
          widget Entry
            [ #overwriteMode := True
            , #text := UUID.toText uuid
            , onM #focusOutEvent onFocusOut
            ]
          where
            uuid = UUID.fromWords a b c d
            onFocusOut :: EventFocus -> Entry -> IO (Bool, Event)
            onFocusOut _ entry = do
              text <- #getText entry
              newUuid <- case UUID.fromText text of
                Just uuid' ->
                  pure uuid'
                Nothing -> do
                  -- the user entered an invalid UUID, so don't save it
                  #setText entry $ UUID.toText uuid
                  pure uuid
              let uuidElem = uncurry4 UUID . UUID.toWords $ newUuid
              pure (False, replaceElemOnChanged uuidElem)

        completeVsInput :: Versionstamp 'Complete -> Widget Event
        completeVsInput (CompleteVersionstamp (TransactionVersionstamp tx batch) usr) =
          container Box [#spacing := 4]
            [ label "Tx:"
            , spin $ component IntegerSpinner.integerSpinner
                { IntegerSpinner.value = tx
                , IntegerSpinner.onChanged = Just (replaceElemOnChanged . CompleteVS . flip CompleteVersionstamp usr . flip TransactionVersionstamp batch)
                }
            , label "Batch:"
            , spin $ component IntegerSpinner.integerSpinner
                { IntegerSpinner.value = batch
                , IntegerSpinner.onChanged = Just (replaceElemOnChanged . CompleteVS . flip CompleteVersionstamp usr . TransactionVersionstamp tx)
                }
            , label "User:"
            , spin $ component IntegerSpinner.integerSpinner
                { IntegerSpinner.value = usr
                , IntegerSpinner.onChanged =  Just (replaceElemOnChanged . CompleteVS . CompleteVersionstamp (TransactionVersionstamp tx batch))
                }
            ]
            where
              label t = BoxChild
                { properties = defaultBoxChildProperties
                , child = widget Label [#label := t]
                }
              spin child = BoxChild
                { properties = defaultBoxChildProperties { expand = True, fill = True }
                , child
                }

        tupleInput :: [EditableElem] -> Widget Event
        tupleInput t =
          tupleEntry' t (replaceElemOnChanged . Tuple)

        replaceElemOnChanged :: EditableElem -> Event
        replaceElemOnChanged = 
          onChanged . replaceElem
        
        replaceElem :: EditableElem -> [EditableElem]
        replaceElem newElem =
          setAt i newElem elems

    addElemButton :: BoxChild Event
    addElemButton =
      BoxChild defaultBoxChildProperties $
        widget Button
          [ #label := "Add Element"
          , #halign := AlignStart
          , on #clicked (onChanged (elems `snoc` SingleLineText ""))
          ]

escapeSyntaxHelp :: Text
escapeSyntaxHelp =
  "Text will be UTF-8 encoded except for byte values specified in hex, like \
   \'\\xA0'. Use double slash '\\\\' to enter a single slash."
