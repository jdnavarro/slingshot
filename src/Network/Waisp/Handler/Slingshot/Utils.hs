module Network.Waisp.Handler.Slingshot.Utils where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

showBS :: Show a => a -> ByteString
showBS = B8.pack . show
