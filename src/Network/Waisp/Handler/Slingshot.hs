{-# LANGUAGE OverloadedStrings #-}
module Network.Waisp.Handler.Slingshot where

import Control.Applicative ((*>))
import Control.Monad (void, forever)
import Data.ByteString.Buildable (build)
import Network.Socket (Socket, close)
import Control.Monad.Trans.State.Strict (runStateT)

import Control.Monad.Catch (bracket)
import Pipes ((>->), runEffect)
import Pipes.Network.TCP (acceptFork, fromSocket, toSocket)
import Pipes.Attoparsec (parse)
import Pipes.ByteString (fromBuilder)

import Network.Waisp
  ( Application(..)
  , Request(..)
  , Response(..)
  )

import Network.Waisp.Handler.Slingshot.Settings
import Network.Waisp.Handler.Slingshot.Request
import Network.Waisp.Handler.Slingshot.Network

type Port = Int

serve :: Port -> Application -> IO r
serve p = serveSettings defaultSettings { settingsPort = p }

serveSettings :: Settings -> Application -> IO r
serveSettings set app = bracket
    (bindPortTCP (settingsPort set) (settingsHost set))
     close
    (\sock -> serveSettingsSocket set sock app)

-- TODO: use `pipes-safe` variant
serveSettingsSocket :: Settings -> Socket -> Application -> IO r
serveSettingsSocket _ sock (Application f) =
    forever . void . acceptFork sock $ \(sock', _) -> do
        mreq <- recvRequest sock'
        case mreq of
             Just req -> f req >>= sendResponse sock'
             Nothing -> return ()

recvRequest :: Socket -> IO (Maybe Request)
recvRequest sock = do
    (mh, b) <- runStateT (parse requestMessageHeaderParser)
                         (fromSocket sock 4096)
    case mh of
         Nothing -> return Nothing
         Just eh  -> case eh of
                          Left _  -> return Nothing
                          Right h -> return . Just $ Request h b

sendResponse :: Socket -> Response -> IO ()
sendResponse sock (Response rm b) = runEffect $ (fromBuilder (build rm) *> b) >-> toSocket sock
