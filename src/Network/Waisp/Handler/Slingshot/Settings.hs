{-# LANGUAGE OverloadedStrings #-}
module Network.Waisp.Handler.Slingshot.Settings where

import Network.Waisp.Handler.Slingshot.Network

data Settings = Settings
  { settingsPort :: Int
  , settingsHost :: HostPreference
  }

defaultSettings :: Settings
defaultSettings = Settings 1234 "*"
