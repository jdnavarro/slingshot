{-# LANGUAGE OverloadedStrings #-}
module Network.Waisp.Handler.Slingshot.Request where

import Control.Applicative -- ((<$), (<$>), (<*>), (<|>))
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Attoparsec.Char8
  -- (Parser, stringCI)
import Network.Waisp
  -- ( RequestMessageHeader(..)
  -- , RequestLine
  -- , RequestHeaders
  -- , Host
  -- )

-- * Attoparsec

requestMessageHeaderParser :: Parser RequestMessageHeader
requestMessageHeaderParser =
    RequestMessageHeader <$> requestLineParser
                         <*> hostParser
                         <*> requestHeadersParser

-- ** Request Line

requestLineParser :: Parser RequestLine
requestLineParser =
    RequestLine <$> methodParser <* char8 ' '
                <*> pathInfoParser
                <*> try queryParser
                <*> httpVersionParser

methodParser :: Parser Method
methodParser = GET     <$ stringCI "get"
           <|> PUT     <$ stringCI "put"
           <|> POST    <$ stringCI "post"
           <|> HEAD    <$ stringCI "head"
           <|> DELETE  <$ stringCI "delete"
           <|> TRACE   <$ stringCI "trace"
           <|> CONNECT <$ stringCI "connect"
           <|> OPTIONS <$ stringCI "options"

pathInfoParser :: Parser PathInfo
pathInfoParser = takeWhile1 (\c -> c /= ' ' || c /= '?')

queryParser :: Parser Query
queryParser = char '?' *> takeWhile1 isSpace

httpVersionParser :: Parser HttpVersion
httpVersionParser = HttpVersion
                <$  stringCI "http/"
                <*> decimal
                <*  char '.'
                <*> decimal

-- * Headers

hostParser :: Parser Host
hostParser = undefined

requestHeadersParser :: Parser RequestHeaders
requestHeadersParser = undefined

-- * Common helpers

crlf :: Parser Word8
crlf = undefined

-- isToken :: Word8 -> Bool
-- isToken w = w <= 127 && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w
