module Hello.Foreign.UUID
  ( UUID
  , fromString
  , genUUID
  , toString
  , validate
  )
  where

import Prelude

import Data.Function.Uncurried (Fn0, Fn1, runFn0, runFn1)
import Data.Maybe (Maybe(..))
import Effect (Effect)

newtype UUID = UUID String

derive newtype instance Eq UUID
instance Show UUID where
  show (UUID v) = "(UUID " <> v <> ")"

toString :: UUID -> String
toString (UUID v) = v

fromString :: String -> Maybe UUID
fromString s = if validate s then Just $ UUID s else Nothing

validate :: String -> Boolean
validate = runFn1 _validate

genUUID :: Effect UUID
genUUID = runFn0 _genUUID

foreign import _validate :: Fn1 String Boolean

foreign import _genUUID :: Fn0 (Effect UUID)