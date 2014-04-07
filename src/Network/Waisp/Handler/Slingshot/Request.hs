{-# LANGUAGE OverloadedStrings #-}
module Network.Waisp.Handler.Slingshot.Request where

import Control.Applicative
  ( (<$)
  , (<$>)
  , (<*)
  , (*>)
  , (<*>)
  , (<|>)
  , pure
  , many
  )
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
-- >>> import Data.Attoparsec (parseTest, parseOnly)

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
methodParser = asum $ meth <$> enumAll
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

{-| Host header parser.

    >>> let bs = "Host: www.example.com\r\n" :: ByteString
    >>> parseTest hostParser bs
    Done "" "www.example.com"
-}
hostParser :: Parser Host
hostParser = stringCI "host:"
          *> skipSpace
          *> takeTill isEndOfLine
          <* endOfLine

{-| Parser for all headers of a 'Request'.

    >>> -- let bs = "Connection: close\r\nAccept: */*\r\n\r\n :: ByteString
    >>> -- parseTest requestHeadersParser bs
-}
requestHeadersParser :: Parser RequestHeaders
-- XXX: Support unordered headers
requestHeadersParser = RequestHeaders <$> headersGeneralParser
                                      <*> headersRequestParser
                                      <*> headersCustomParser
                                      <*  endOfLine

{-| Parse general headers of a 'Request'.

    >>> let bs = "Connection: close\r\nDate: Sun, 18 Oct 2009 08:56:53 GMT\r\nAccept-Language: en-us\r\n" :: ByteString
    >>> parseTest headersGeneralParser bs
    Done "Accept-Language: en-us\r\n" fromList [(Connection,"close"),(Date,"Sun, 18 Oct 2009 08:56:53 GMT")]

    >>> let bs' = "Connection: close\r\nAccept-Language\r\n" :: ByteString
    >>> parseTest headersGeneralParser bs'
    Done "Accept-Language\r\n" fromList [(Connection,"close")]
-}
headersGeneralParser :: Parser (Headers HeaderGeneral)
headersGeneralParser = headersParser headerGeneralParser

{-| Parse a single general header.

    >>> let bs = "Connection: close\r\n" :: ByteString
    >>> parseTest headerGeneralParser bs
    Done "" (Connection,"close")
    >>> let bs' = "Date: Sun, 18 Oct 2009 08:56:53 GMT\r\n" :: ByteString
    >>> parseTest headerGeneralParser bs'
    Done "" (Date,"Sun, 18 Oct 2009 08:56:53 GMT")
-}
headerGeneralParser :: Parser (HeaderGeneral, ByteString)
headerGeneralParser = headerParser

{-| Parse the specific headers of a 'Request'.

    >>> let bs = "Accept: */*\r\nAccept-Language: en-us\r\nConnection: close\r\n" :: ByteString
    >>> parseTest headersRequestParser bs
    Done "Connection: close\r\n" fromList [(Accept,"*/*"),(Accept-Language,"en-us")]
-}
headersRequestParser :: Parser (Headers HeaderRequest)
headersRequestParser = headersParser headerRequestParser

{-| Parse a single specific header of a 'Request'.

    >>> let bs = "Accept: */*\r\n" :: ByteString
    >>> parseTest headerRequestParser bs
    Done "" (Accept,"*/*")
-}
headerRequestParser :: Parser (HeaderRequest, ByteString)
headerRequestParser = headerParser

{-| Parse custom headers of a 'Request'.

    >>> let bs = "SomeHeader: SomeValue\r\nAnotherHeader: AnotherValue\r\n" :: ByteString
    >>> parseOnly headersCustomParser bs
    Right (fromList [("AnotherHeader","AnotherValue"),("SomeHeader","SomeValue")])
-}
headersCustomParser :: Parser (Headers HeaderCustom)
headersCustomParser = headersParser headerCustomParser

{-| Parse a single custom header of a 'Request'.

    >>> let bs = "SomeHeader: SomeValue\r\n" :: ByteString
    >>> parseTest headerCustomParser bs
    Done "" ("SomeHeader","SomeValue")
-}
headerCustomParser :: Parser (HeaderCustom, ByteString)
headerCustomParser = do
    field <- takeWhile1 (/= ':') <* char ':'
    value <- skipSpaces *> takeTill isEndOfLine <* endOfLine
    return (field, value)

-- ** Header helpers

headersParser :: (Show h, Ord h)
              => Parser (h, ByteString)
              -> Parser (Headers h)
headersParser = fmap Map.fromList . many

headerParser :: (Show h, Enum h) => Parser (h, ByteString)
headerParser = asum $ mkHeaderParser <$> enumAll

mkHeaderParser :: Show h => h -> Parser (h, ByteString)
mkHeaderParser h = stringCI (showBS h)
                *> char ':'
                *> skipSpaces
                *> do bs <- takeTill isEndOfLine <* endOfLine
                      return (h, bs)

-- * Common helpers

showBS :: Show a => a -> ByteString
showBS = B8.pack . show

skipSpaces :: Parser ()
skipSpaces = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace

enumAll :: Enum a => [a]
enumAll = enumFrom $ toEnum 0
