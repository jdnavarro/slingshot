{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import qualified Data.Map as Map
import Pipes (yield)

import Network.Waisp
import Network.Waisp.Handler.Slingshot

app :: Application
app = Application
    . const
    $ putStrLn "Doing some IO"
   $> Response status respHs body
  where
    status = StatusLine (HttpVersion 1 1) Status200
    respHs = ResponseHeaders Map.empty Map.empty (Map.fromList [(ContentType, "text/plain")]) Map.empty
    body   = yield "Hello Web!"

main :: IO r
main = putStrLn "http://localhost:8080/"
    *> serve 8080 app

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
