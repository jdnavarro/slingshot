{-# LANGUAGE OverloadedStrings #-}
module Network.Waisp.Handler.Slingshot.Request where

import Control.Applicative -- ((<$), (<$>), (<*>), (<|>))
import Data.Foldable (asum)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import Data.Attoparsec (satisfy, skipWhile, takeTill)
import Data.Attoparsec.Char8
  ( Parser
  , stringCI
  , decimal
  , try
  , char
  , takeWhile1
  , skipSpace
  , isEndOfLine
  , endOfLine
  , isHorizontalSpace
  )
import Network.Waisp
  -- ( RequestMessageHeader(..)
  -- , RequestLine
  -- , RequestHeaders
  -- , Host
  -- )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.ByteString.Char8
-- >>> import Data.Attoparsec (parseTest)

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
                <*  skipSpaces
                <*> pathInfoParser
                <*> (try queryParser <|> pure B.empty)
                <*  skipSpaces
                <*> httpVersionParser
                <*  endOfLine
{-|

>>> let bs = "GET /docs/index.html" :: ByteString
>>> parseTest methodParser bs
Done " /docs/index.html" GET
-}
methodParser :: Parser Method
methodParser = asum $ meth <$> enumFrom OPTIONS
  where
    meth met = met <$ stringCI (showBS met)

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
queryParser = char '?' *> takeTill isHorizontalSpace

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
hostParser = stringCI "host:" *> skipSpace *> takeTill isEndOfLine <* endOfLine

requestHeadersParser :: Parser RequestHeaders
requestHeadersParser = RequestHeaders
    <$> headersGeneralParser
    <*> headersRequestParser
    <*> headersCustomParser
    <*  endOfLine

headersGeneralParser :: Parser (Headers HeaderGeneral)
headersGeneralParser = Map.fromList <$> many headerGeneralParser

headerGeneralParser :: Parser (HeaderGeneral, ByteString)
headerGeneralParser = asum $ mkHeaderParser <$> enumFrom CacheControl

headersRequestParser :: Parser (Headers HeaderRequest)
headersRequestParser = undefined

headersCustomParser :: Parser (Headers HeaderCustom)
headersCustomParser = undefined

-- * Common helpers

mkHeaderParser :: Show header => header -> Parser (header, ByteString)
mkHeaderParser h = stringCI (showBS h)
                *> char ':'
                *> skipSpaces
                *> do bs <- takeTill isEndOfLine <* endOfLine
                      return (h, bs)

showBS :: Show a => a -> ByteString
showBS = B8.pack . show

skipSpaces :: Parser ()
skipSpaces = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace
