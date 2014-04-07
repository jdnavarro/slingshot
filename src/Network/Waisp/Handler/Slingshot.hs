module Network.Waisp.Handler.Slingshot where

import Control.Monad (void, forever)
import Network.Socket (Socket, close)
import Control.Monad.Catch (bracket)
import Pipes ((>->), runEffect)
import Pipes.Network.TCP (acceptFork, fromSocket, toSocket)
import Network.Waisp (Application)
import Network.Waisp.Handler.Slingshot.Settings
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
serveSettingsSocket set sock app =
    forever . void . acceptFork sock $ \(sock', _) ->
        runEffect $ fromSocket sock' 4096
                >-> undefined
                >-> toSocket sock'
