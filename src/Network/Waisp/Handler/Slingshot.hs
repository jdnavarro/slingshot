module Network.Waisp.Handler.Slingshot where

import Network.Socket (Socket, close)
import Control.Monad.Catch (bracket)
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
    (\socket -> serveSettingsSocket set socket app)

serveSettingsSocket :: Settings -> Socket -> Application -> IO r
serveSettingsSocket = undefined
