{-# LANGUAGE RankNTypes #-}
module Network.Waisp.Handler.Slingshot.Request where

import Control.Applicative ((<$>), (<*>))
import Data.Word (Word8)
import Data.Attoparsec (Parser)
import Network.Waisp
  ( RequestMessageHeader(..)
  , RequestLine
  , RequestHeaders
  , Host
  )

-- * Attoparsec

requestMessageHeaderParser :: Parser RequestMessageHeader
requestMessageHeaderParser =
    RequestMessageHeader <$> requestLineParser
                         <*> hostParser
                         <*> requestHeadersParser

requestLineParser :: Parser RequestLine
requestLineParser = undefined

hostParser :: Parser Host
hostParser = undefined

requestHeadersParser :: Parser RequestHeaders
requestHeadersParser = undefined

-- * Helpers

crlf :: Parser Word8
crlf = undefined
