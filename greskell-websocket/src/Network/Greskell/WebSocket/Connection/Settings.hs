-- |
-- Module: Network.Greskell.WebSocket.Connection.Settings
-- Description: Settings for making Connection
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Network.Greskell.WebSocket.Connection.Settings
  ( -- * Settings
    Settings,
    defSettings,
    defJSONSettings,
    -- ** accessor functions
    codec, endpointPath, onGeneralException, responseTimeout,
    concurrency, requestQueueSize
  ) where

import Data.Greskell.GraphSON (FromGraphSON)

import Network.Greskell.WebSocket.Codec (Codec)
import Network.Greskell.WebSocket.Codec.JSON (jsonCodec)
import Network.Greskell.WebSocket.Connection.Type (GeneralException)

import System.IO (stderr, hPutStrLn)

-- | 'Settings' for making connection to Gremlin Server.
--
-- You can get the default 'Settings' by 'defSettings' function, and
-- customize its fields by accessor functions.
--
-- Type @s@ is the body of Response.
data Settings s =
  Settings
  { codec :: !(Codec s),
    -- ^ codec for the connection.
    endpointPath :: !String,
    -- ^ Path of the WebSocket endpoint. Default: \"/gremlin\"
    onGeneralException :: !(GeneralException -> IO ()),
    -- ^ An exception handler for 'GeneralException'. This exception
    -- is not fatal, so the connection survives after this handler is
    -- called. You don't have to re-throw the exception. Default:
    -- print the exception to stderr.
    responseTimeout :: !Int,
    -- ^ Time out (in seconds) for responses. It is the maximum time
    -- for which the connection waits for a response to complete after
    -- it sends a request. If the response consists of more than one
    -- ResponseMessages, the timeout applies to the last of the
    -- ResponseMessages. Default: 60
    concurrency :: !Int,
    -- ^ Maximum concurrent requests the connection can make to the
    -- server. If the client tries to make more concurrent requests
    -- than this value, later requests are queued in the connection or
    -- the client may be blocked. Default: 4
    requestQueueSize :: !Int
    -- ^ Size of the internal queue of requests. Usually you don't
    -- need to customize the field. See also 'concurrency'. Default:
    -- 8.
  }

defSettings :: Codec s -> Settings s
defSettings c = Settings
                { codec = c,
                  endpointPath = "/gremlin",
                  onGeneralException = \e -> hPutStrLn stderr $ show e,
                  responseTimeout = 60,
                  concurrency = 4,
                  requestQueueSize = 8
                }

-- | 'defSettings' with 'jsonCodec'.
defJSONSettings :: FromGraphSON s => Settings s
defJSONSettings = defSettings jsonCodec
