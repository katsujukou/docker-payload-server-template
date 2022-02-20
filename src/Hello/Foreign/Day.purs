module Hello.Foreign.Day
  ( DateTime
  , Format(..)
  , format
  , fromString
  , now
  , parseFormat
  , parseFormatStrict
  , toString
  , toUnix
  , toUnixMilliseconds
  )
  where

import Prelude

import Data.Function.Uncurried (Fn0, Fn1, Fn2, Fn5, runFn0, runFn1, runFn2, runFn5)
import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import data DateTime :: Type

instance Eq DateTime where
  eq lhs rhs = (toUnixMilliseconds lhs) == (toUnixMilliseconds rhs)

instance Ord DateTime where
  compare lhs rhs = (toUnixMilliseconds lhs) `compare` (toUnixMilliseconds rhs)

instance Show DateTime where
  show = toString

toUnix :: DateTime -> Int
toUnix = runFn1 _toUnix

toUnixMilliseconds :: DateTime -> Int
toUnixMilliseconds = runFn1 _toUnixMilliseconds

toString :: DateTime -> String
toString = format (Format "YYYY-MM-DDThh:mm:ss.SSSZ")

fromString :: String -> Maybe DateTime
fromString = parseFormat (Format "YYYY-MM-DDThh:mm:ss.SSSZ")

now :: Effect DateTime
now = runFn0 _now

newtype Format = Format String

format :: Format -> DateTime -> String
format (Format f) = runFn2 _format f

parseFormat :: Format -> String -> Maybe DateTime
parseFormat (Format f) = runFn5 _parseFormat Nothing Just f false

parseFormatStrict :: Format -> String -> Maybe DateTime
parseFormatStrict (Format f) = runFn5 _parseFormat Nothing Just f true

foreign import _toUnix :: Fn1 DateTime Int

foreign import _toUnixMilliseconds :: Fn1 DateTime Int

foreign import _parseFormat ::
  Fn5
    (forall a. Maybe a)
    (forall a. a -> Maybe a)
    String
    Boolean
    String
    (Maybe DateTime)

foreign import _now :: Fn0 (Effect DateTime)

foreign import _format :: Fn2 String DateTime String