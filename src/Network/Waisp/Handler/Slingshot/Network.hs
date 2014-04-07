module Network.Waisp.Handler.Slingshot.Network where

import Data.String (IsString(fromString))
import Network.Socket (Socket)

-- | Attempt to bind a listening @Socket@ on the given host/port. If no host is
-- given, will use the first address available.
-- 'maxListenQueue' is topically 128 which is too short for
-- high performance servers. So, we specify 'max 2048 maxListenQueue' to
-- the listen queue.
bindPortTCP :: Int -> HostPreference -> IO Socket
bindPortTCP p s = undefined

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
