module Hello.Server.Core.Email
  ( Email
  , codec
  , fromString
  , toString
  )
  where

import Prelude

import Control.Monad.Except (except, runExcept)
import Data.Codec.Argonaut (prismaticCodec)
import Data.Codec.Argonaut as CA
import Data.Either (hush, note)
import Data.Lens (_Left, over)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Semigroup.Foldable (intercalateMap)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Database.PostgreSQL (class FromSQLValue)
import Foreign (ForeignError(..), readString, renderForeignError)
import Simple.JSON (class ReadForeign, read')

newtype Email = Email String

derive newtype instance Eq Email
derive newtype instance Ord Email

instance Show Email where
  show (Email em) = "(Email " <> em <> ")"

instance ReadForeign Email where
  readImpl = readString
    >=> (fromString >>> note (NEL.singleton $ ForeignError "Invalid email"))
    >>> except

instance FromSQLValue Email where
  fromSQLValue = read' >>> runExcept >>> (over _Left (intercalateMap "\n" renderForeignError))

codec :: CA.JsonCodec Email
codec = prismaticCodec "Email" fromString toString CA.string

toString :: Email -> String
toString (Email em) = em

fromString :: String -> Maybe Email
fromString s = do
  pattern <- hush $ regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$" noFlags
  if pattern `test` s
    then Just $ Email s
    else Nothing

