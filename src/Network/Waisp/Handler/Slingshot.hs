{-# LANGUAGE OverloadedStrings #-}
module Network.Waisp.Handler.Slingshot where

import Control.Applicative ((*>))
import Control.Monad (void, forever)
import Data.Monoid ((<>))
import Data.Foldable (foldMap)
import Data.ByteString (ByteString)
import qualified Data.Map as Map (toList)
import Network.Socket (Socket, close)
import Control.Monad.Trans.State.Strict (runStateT)

import Control.Monad.Catch (bracket)
import Pipes ((>->), runEffect, yield)
import Pipes.Network.TCP (acceptFork, fromSocket, toSocket)
import Pipes.Attoparsec (parse)

import Network.Waisp (Application(..), Request(..), Response(..), ResponseHeaders(..), Body)
import Network.Waisp.Handler.Slingshot.Settings
import Network.Waisp.Handler.Slingshot.Request
import Network.Waisp.Handler.Slingshot.Network
import Network.Waisp.Handler.Slingshot.Utils
import Debug.Trace

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
    (mh, body) <- runStateT (parse requestMessageHeaderParser) (fromSocket sock 4096)
    case mh of
         Nothing -> return Nothing
         Just eh  -> case eh of
                          Left _  -> return Nothing
                          Right h -> return . Just $ Request h body

sendResponse :: Socket -> Response -> IO ()
sendResponse sock res =
    runEffect $ (yield (responseMessageHeader res) *> responseBody res) >-> toSocket sock

responseMessageHeader :: Response -> ByteString
responseMessageHeader (Response statusline headers _) =
    showBS statusline <> showHeaders headers
  where
    showHeaders (ResponseHeaders h0 h1 h2 h3) = format h0 <> format h1 <> format h2 <> format h3 <> "\r\n"
    format m = foldMap format' $ Map.toList m
    format' (h,f) = showBS h <> ": " <> showBS f <> "\r\n"

responseBody :: Response -> Body
responseBody (Response _ _ body) = body
