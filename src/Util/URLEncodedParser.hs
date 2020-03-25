module Util.URLEncodedParser where

import Control.Arrow (second, (|||))
import Data.Char (chr)
import Numeric (readHex)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as Ascii
import Data.Text hiding (map,find)
import Data.List (map, find)

type QueryItem = (B.ByteString, Maybe B.ByteString)
type Query     = [QueryItem]


liftMaybe item = case item of 
    Just x  -> 
      case x of 
        Just x  -> return  x
    Nothing -> fail "no field"

getFromQuery (txt, id, name) query = do
    txt  <- liftMaybe $ search txt  query
    id   <- liftMaybe $ search id   query
    name <- liftMaybe $ search name query
    pure [txt, id, name]

search :: Eq a => a -> [(a,b)] -> Maybe b
search a = fmap snd . find ((== a) . fst)

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
      parseQuery' = map parseQueryItem . Ascii.split '&'
      parseQueryItem q = (k, v)
        where (k', v') = Ascii.break (== '=') q
              k = urlDecode k'
              v = if B.null v'
                    then Nothing
                    else Just $ urlDecode $ B.tail v'
