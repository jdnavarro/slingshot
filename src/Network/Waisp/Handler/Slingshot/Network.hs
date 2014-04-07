module Network.Waisp.Handler.Slingshot.Network where

-- | XXX: Ripped off from 'streaming-commons', give proper credit.

-- TODO: This should be in another package and, possible, everything but
-- bindPortTCP should be exported through an internal module.

import Data.Monoid ((<>))
import Data.Foldable (traverse_)
import Data.String (IsString(fromString))
import Network.Socket
  ( Socket
  , SocketType(Stream, Datagram)
  , AddrInfoFlag(AI_PASSIVE, AI_NUMERICSERV, AI_NUMERICHOST)
  , Family(AF_INET6)
  , SocketOption(ReuseAddr, NoDelay)
  , socket
  , listen
  , addrProtocol
  , maxListenQueue
  , defaultHints
  , addrFlags
  , addrSocketType
  , getAddrInfo
  , addrFamily
  , close
  , setSocketOption
  , bindSocket
  , addrAddress
  )
import Control.Monad.Catch
  ( catchIOError
  , bracketOnError
  )

-- | Attempt to bind a listening @Socket@ on the given host/port. If no host is
-- given, will use the first address available.
-- 'maxListenQueue' is topically 128 which is too short for
-- high performance servers. So, we specify 'max 2048 maxListenQueue' to
-- the listen queue.
bindPortTCP :: Int -> HostPreference -> IO Socket
bindPortTCP p s =  do
    sock <- bindPort Stream p s
    listen sock $ max 2048 maxListenQueue
    return sock

bindPort :: SocketType -> Int -> HostPreference -> IO Socket
bindPort sockettype p s = do
    let hints = defaultHints
            { addrFlags = [ AI_PASSIVE
                          , AI_NUMERICSERV
                          , AI_NUMERICHOST
                          ]
            , addrSocketType = sockettype
            }
        host = case s of
                    Host s' -> Just s'
                    _       -> Nothing
        port = Just . show $ p
    addrs <- getAddrInfo (Just hints) host port
    -- Choose an IPv6 socket if exists. This ensures the socket can
    -- handle both IPv4 and IPv6 if v6only is false.
    let addrs4 = filter (\x -> addrFamily x /= AF_INET6) addrs
        addrs6 = filter (\x -> addrFamily x == AF_INET6) addrs
        addrs' = case s of
                      HostIPv4     -> addrs4 <> addrs6
                      HostIPv4Only -> addrs4
                      HostIPv6     -> addrs6 <> addrs4
                      HostIPv6Only -> addrs6
                      _            -> addrs

        tryAddrs (addr1:rest@(_:_)) = catchIOError (theBody addr1)
                                                   (const $ tryAddrs rest)
        tryAddrs (addr1:[]) = theBody addr1
        tryAddrs _ = error "bindPort: addrs is empty"

        sockOpts = case sockettype of
                        Datagram -> [(ReuseAddr,1)]
                        _        -> [(NoDelay,1), (ReuseAddr,1)]

        theBody addr =
            bracketOnError
            (socket (addrFamily addr)
                    (addrSocketType addr)
                    (addrProtocol addr))
            close
            (\sock -> do traverse_ (uncurry $ setSocketOption sock) sockOpts
                         bindSocket sock (addrAddress addr)
                         return sock)
    tryAddrs addrs'

-- | Which host to bind.
--
-- Note: The @IsString@ instance recognizes the following special values:
--
-- * @*@ means @HostAny@
--
-- * @*4@ means @HostIPv4@
--
-- * @!4@ means @HostIPv4Only@
--
-- * @*6@ means @HostIPv6@
--
-- * @!6@ means @HostIPv6Only@
data HostPreference =
    HostAny
  | HostIPv4
  | HostIPv4Only
  | HostIPv6
  | HostIPv6Only
  | Host String
    deriving (Eq, Ord, Show, Read)

instance IsString HostPreference where
    -- The funny code coming up is to get around some irritating warnings from
    -- GHC. I should be able to just write:
    {-
fromString "*" = HostAny
fromString "*4" = HostIPv4
fromString "!4" = HostIPv4Only
fromString "*6" = HostIPv6
fromString "!6" = HostIPv6Only
-}
    fromString s'@('*':s) =
        case s of
            [] -> HostAny
            ['4'] -> HostIPv4
            ['6'] -> HostIPv6
            _ -> Host s'
    fromString s'@('!':s) =
        case s of
            ['4'] -> HostIPv4Only
            ['6'] -> HostIPv6Only
            _ -> Host s'
    fromString s = Host s
