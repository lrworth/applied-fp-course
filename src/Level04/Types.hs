{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Level04.Types
  ( Error (..),
    RqType (..),
    ContentType (..),
    Topic,
    CommentText,
    Comment (..),
    mkTopic,
    encodeComment,
    encodeTopic,
    getTopic,
    mkCommentText,
    getCommentText,
    renderContentType,
    fromDBComment,
  )
where

import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import qualified Data.Time.Format as TF
import GHC.Generics (Generic)
import Level04.DB.Types (DBComment (..))
-- Notice how we've moved these types into their own modules. It's cheap and
-- easy to add modules to carve out components in a Haskell application. So
-- whenever you think that a module is too big, covers more than one piece of
-- distinct functionality, or you want to carve out a particular piece of code,
-- just spin up another module.
import Level04.Types.CommentText
  ( CommentText,
    encodeCommentText,
    getCommentText,
    mkCommentText,
  )
import Level04.Types.Error (Error (..))
import Level04.Types.Topic (Topic, encodeTopic, getTopic, mkTopic)
import Waargonaut.Encode (Encoder)
import qualified Waargonaut.Encode as E

newtype CommentId = CommentId Int
  deriving (Eq, Show)

mkCommentId ::
  Int ->
  Either Error CommentId
mkCommentId = Right . CommentId

getCommentId ::
  CommentId ->
  Int
getCommentId (CommentId t) =
  t

encodeCommentId :: Applicative f => Encoder f CommentId
encodeCommentId = getCommentId >$< E.int

-- | This is the `Comment` record that we will be sending to users, it's a
-- straightforward record type, containing an `Int`, `Topic`, `CommentText`, and
-- `UTCTime`.
data Comment
  = Comment
      { commentId :: CommentId,
        commentTopic :: Topic,
        commentBody :: CommentText,
        commentTime :: UTCTime
      }
  deriving (Show)

-- | We're going to write the JSON encoder for our `Comment` type. We'll need to
-- consult the documentation in the 'Waargonaut.Encode' module to find the
-- relevant functions and instructions on how to use them:
--
-- 'https://hackage.haskell.org/package/waargonaut/docs/Waargonaut-Encode.html'
encodeComment :: Applicative f => Encoder f Comment
encodeComment = E.mapLikeObj $ \comment ->
  (E.atKey' "id" encodeCommentId . commentId $ comment)
    . (E.atKey' "topic" encodeTopic . commentTopic $ comment)
    . (E.atKey' "body" encodeCommentText . commentBody $ comment)
    . (E.atKey' "time" encodeISO8601DateTime . commentTime $ comment)

-- | For safety we take our stored `DBComment` and try to construct a `Comment`
-- that we would be okay with showing someone. However unlikely it may be, this
-- is a nice method for separating out the back and front end of a web app and
-- providing greater guarantees about data cleanliness.
fromDBComment ::
  DBComment ->
  Either Error Comment
fromDBComment c =
  Comment <$> mkCommentId (dbCommentId c) <*> mkTopic (dbCommentTopic c) <*> mkCommentText (dbCommentBody c) <*> Right (dbCommentTime c)

data RqType
  = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

data ContentType
  = PlainText
  | JSON

renderContentType ::
  ContentType ->
  ByteString
renderContentType PlainText = "text/plain"
renderContentType JSON = "application/json"

encodeISO8601DateTime :: Applicative f => Encoder f UTCTime
encodeISO8601DateTime = pack . TF.formatTime loc fmt >$< E.text
  where
    fmt = TF.iso8601DateFormat (Just "%H:%M:%S")
    loc = TF.defaultTimeLocale {TF.knownTimeZones = []}

-- | Move on to ``src/Level04/DB.hs`` next.
