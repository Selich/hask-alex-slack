module Util.URLEncodedParser where

import           Control.Arrow         (second, (|||))
import           Data.Array
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.String
import           Numeric
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as Ascii
import Data.Text.Encoding
import Data.Text

type QueryItem = (B.ByteString, Maybe B.ByteString)
type Query     = [QueryItem]

urlDecode :: B.ByteString -> B.ByteString
urlDecode bs = case Ascii.uncons bs of
    Just ('%', x) -> case readHex $ Ascii.unpack pc of
        [(v, "")] -> chr v `Ascii.cons` urlDecode bs'
        _ -> Ascii.cons '%' $ urlDecode x
        where (pc, bs') = Ascii.splitAt 2 x
    Just (c, bs') -> Ascii.cons c $ urlDecode bs'
    Nothing -> B.empty

parseQuery :: B.ByteString -> Query
parseQuery bs = case Ascii.uncons bs of
        Nothing         -> []
        Just ('?', bs') -> parseQuery' bs'
        _               -> parseQuery' bs
    where
      parseQuery' = Data.List.map parseQueryItem . Ascii.split '&'
      parseQueryItem q = (k, v)
        where (k', v') = Ascii.break (== '=') q
              k = urlDecode k'
              v = if B.null v'
                    then Nothing
                    else Just $ urlDecode $ B.tail v'
