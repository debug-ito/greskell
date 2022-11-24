-- |
-- Module: Network.Greskell.WebSocket
-- Description: Client of Gremlin Server using WebSocket
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module Network.Greskell.WebSocket
    ( -- $doc
      module Network.Greskell.WebSocket.Client
    ) where

import           Network.Greskell.WebSocket.Client

-- $doc
--
-- Client for Gremlin Server using the WebSocket serializer. For
-- example, see the project
-- [README.md](https://github.com/debug-ito/greskell#submit-to-the-gremlin-server)
--
-- End-users usually only have to use
-- "Network.Greskell.WebSocket.Client", so this module re-exports only
-- that module.
--
-- Other modules are low-level implementation and for advanced uses.
--
-- - "Network.Greskell.WebSocket.Connection": Connection to the
--   Gremlin Server implementing the Driver protocol described in
--   <http://tinkerpop.apache.org/docs/current/dev/provider/>.
-- - "Network.Greskell.WebSocket.Codec": Encoder and decoder of
--    RequestMessage and ResponseMessage.
-- - "Network.Greskell.WebSocket.Request": RequestMessage object sent
--   to Gremlin Server.
-- - "Network.Greskell.WebSocket.Request.Standard": Request objects
--   for Standard OpProcessor.
-- - "Network.Greskell.WebSocket.Request.Session": Request objects for
--   Session OpProcessor.
-- - "Network.Greskell.WebSocket.Response": ResponseMessage object
--   returned from Gremlin Server.

