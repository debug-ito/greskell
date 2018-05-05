-- |
-- Module: Data.Greskell.WebSocket.Client
-- Description: High-level interface to Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Client
       ( Client,
         connect,
         close,
         submitRaw,
         nextResult,
         ResultHandle
       ) where

import Data.Aeson (Object, Value)
import Data.Text (Text)

import Data.Greskell.WebSocket.Client.Settings (Settings)
import Data.Greskell.WebSocket.Connection
  ( Host, Port
  )


-- | A client that establishes a connection to the Gremlin Server. You
-- can send Gremlin expression for evaluation by 'submit' function.
data Client

-- | Create a 'Client' to a Gremlin Server.
connect :: Settings -> Host -> Port -> IO Client
connect = undefined

-- | Close the connection to the server and release other resources of
-- the 'Client'.
close :: Client -> IO ()
close = undefined

-- | A handle to receive the result of evaluation of a Gremlin script
-- from the server.
data ResultHandle v

-- | Less type-safe version of 'submit'.
submitRaw :: Client
          -> Text -- ^ Gremlin script
          -> Maybe Object -- ^ bindings
          -> IO (ResultHandle Value)
submitRaw = undefined

-- | Get the next value from the 'ResultHandle'. If you have got all
-- values, it returns 'Nothing'.
nextResult :: ResultHandle v -> IO (Maybe v)
nextResult = undefined
