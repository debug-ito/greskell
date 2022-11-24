-- |
-- Module: Network.Greskell.WebSocket.Client.Options
-- Description: Options to create a Client
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
--
module Network.Greskell.WebSocket.Client.Options
    ( -- * Options
      Options
    , defOptions
      -- ** accessor functions
    , connectionSettings
    , batchSize
    , language
    , aliases
    , scriptEvaluationTimeout
      -- * Settings
    , module Network.Greskell.WebSocket.Connection.Settings
    ) where

import           Data.Greskell.GraphSON                         (GValue)
import           Data.HashMap.Strict                            (HashMap)
import           Data.Text                                      (Text)

import           Network.Greskell.WebSocket.Connection          (Connection)
import           Network.Greskell.WebSocket.Connection.Settings

-- | Configuration options to create a client for Gremlin Server.
--
-- You can get the default 'Options' by 'defOptions' function, and
-- customize its fields by accessor functions.
data Options
  = Options
      { connectionSettings      :: !(Settings GValue)
        -- ^ Settings for the underlying 'Connection'. Default:
        -- 'defJSONSettings'.
      , batchSize               :: !(Maybe Int)
        -- ^ \"batchSize\" field for \"eval\" operation. Default:
        -- 'Nothing'.
      , language                :: !(Maybe Text)
        -- ^ \"language\" field for \"eval\" operation. Default:
        -- 'Nothing'.
      , aliases                 :: !(Maybe (HashMap Text Text))
        -- ^ \"aliases\" field for \"eval\" operation. Default: 'Nothing'.
      , scriptEvaluationTimeout :: !(Maybe Int)
        -- ^ \"scriptEvaluationTimeout\" field for \"eval\"
        -- operation. Default: 'Nothing'.
      }

defOptions :: Options
defOptions =
  Options
  { connectionSettings = defJSONSettings,
    batchSize = Nothing,
    language = Nothing,
    aliases = Nothing,
    scriptEvaluationTimeout = Nothing
  }
