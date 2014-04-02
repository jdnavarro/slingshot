module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-isrc", "src/Network/Waisp/Handler/Slingshot/Request.hs"]
