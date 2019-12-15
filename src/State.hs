module State (State (..)) where

import Data.Text (Text)

data State = ChoosingClusterFile{ selectedClusterFile :: Maybe Text }
           | ChosenClusterFile{ clusterFilePath :: Text, status :: Text }