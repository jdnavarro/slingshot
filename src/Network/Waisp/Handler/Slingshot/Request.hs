{-# LANGUAGE OverloadedStrings #-}
module Network.Waisp.Handler.Slingshot.Request where

import Control.Applicative -- ((<$), (<$>), (<*>), (<|>))
import Data.Word (Word8)
import qualified Data.ByteString as B
import Data.Attoparsec.Char8
  -- (Parser, stringCI)
import Network.Waisp
  -- ( RequestMessageHeader(..)
  -- , RequestLine
  -- , RequestHeaders
  -- , Host
  -- )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.ByteString.Char8

-- * Attoparsec

requestMessageHeaderParser :: Parser RequestMessageHeader
requestMessageHeaderParser =
    RequestMessageHeader <$> requestLineParser
                         <*> hostParser
                         <*> requestHeadersParser

-- ** Request Line

{-|

    >>> let bs = "GET /docs/index.html?query=value HTTP/1.1\r\n" :: ByteString
    >>> parseTest requestLineParser bs
    Done "" RequestLine GET "/docs/index.html" "query=value" (HttpVersion 1 1)

    >>> let bs' = "GET /docs/index.html HTTP/1.1\r\n" :: ByteString
    >>> parseTest requestLineParser bs'
    Done "" RequestLine GET "/docs/index.html" "" (HttpVersion 1 1)
-}
requestLineParser :: Parser RequestLine
requestLineParser =
    RequestLine <$> methodParser
                <*  char ' '
                <*> pathInfoParser
                <*> (try queryParser <|> pure B.empty)
                <*  char ' '
                <*> httpVersionParser
                <*  string "\r\n"
{-|

>>> let bs = "GET /docs/index.html" :: ByteString
>>> parseTest methodParser bs
Done " /docs/index.html" GET
-}
methodParser :: Parser Method
methodParser = GET     <$ stringCI "get"
           <|> PUT     <$ stringCI "put"
           <|> POST    <$ stringCI "post"
           <|> HEAD    <$ stringCI "head"
           <|> DELETE  <$ stringCI "delete"
           <|> TRACE   <$ stringCI "trace"
           <|> CONNECT <$ stringCI "connect"
           <|> OPTIONS <$ stringCI "options"

{-| 'PathInfo' parser.

    It stops parsing after a /space/ or a @?@

    >>> let bs = "/docs/index.html?query=value" :: ByteString
    >>> parseTest pathInfoParser bs
    Done "?query=value" "/docs/index.html"
-}
pathInfoParser :: Parser PathInfo
pathInfoParser = takeWhile1 (\c -> c /= ' ' && c /= '?')

{-| 'Query' parser.

    The query string is expected to start with @?@.

    >>> let bs = "?query=value HTTP/1.1" :: ByteString
    >>> parseTest queryParser bs
    Done " HTTP/1.1" "query=value"
-}
queryParser :: Parser Query
queryParser = char '?' *> takeWhile1 (/= ' ')

{-| 'HttpVersion' parser.

    The trailing @CRLF@ is not consumed.

    >>> let bs = "HTTP/1.1\r\n" :: ByteString
    >>> parseTest httpVersionParser bs
    Done "\r\n" HttpVersion 1 1
-}
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
