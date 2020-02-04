{-# LANGUAGE OverloadedStrings #-}

module Level05.DB
  ( FirstAppDB (FirstAppDB),
    initDB,
    closeDB,
    addCommentToTopic,
    getComments,
    getTopics,
    deleteTopic,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Database.SQLite.Simple
  ( Connection,
    Query (fromQuery),
  )
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.SimpleErrors as Sql
import Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import Level05.AppM (AppM, liftEither)
import Level05.Types
  ( Comment,
    CommentText,
    Error (DBError),
    Topic,
    fromDBComment,
    getCommentText,
    getTopic,
    mkTopic,
  )

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB
  = FirstAppDB
      { dbConn :: Connection
      }

-- Quick helper to pull the connection and close it down.
closeDB ::
  FirstAppDB ->
  IO ()
closeDB =
  Sql.close . dbConn

initDB ::
  FilePath ->
  IO (Either SQLiteResponse FirstAppDB)
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
    -- Query has an `IsString` instance so string literals like this can be
    -- converted into a `Query` type when the `OverloadedStrings` language
    -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB ::
  (a -> Either Error b) ->
  IO a ->
  AppM b
runDB f io = do
  -- This function is intended to abstract away the running of DB functions and
  -- the catching of any errors. As well as the process of running some
  -- processing function over those results.
  respE <- liftIO (Sql.runDBAction io)
  a <- liftEither (first DBError respE)
  liftEither (f a)

getComments ::
  FirstAppDB ->
  Topic ->
  AppM [Comment]
getComments db topic =
  let sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
   in -- There are several possible implementations of this function. Particularly
      -- there may be a trade-off between deciding to throw an Error if a DBComment
      -- cannot be converted to a Comment, or simply ignoring any DBComment that is
      -- not valid.
      runDB (traverse fromDBComment) (Sql.query (dbConn db) sql (Sql.Only (getTopic topic)))

addCommentToTopic ::
  FirstAppDB ->
  Topic ->
  CommentText ->
  AppM ()
addCommentToTopic db topic commentText =
  let sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
   in do
        time <- liftIO getCurrentTime
        runDB pure (Sql.execute (dbConn db) sql (getTopic topic, getCommentText commentText, time))

getTopics ::
  FirstAppDB ->
  AppM [Topic]
getTopics db =
  let sql = "SELECT DISTINCT topic FROM comments"
   in runDB (traverse (mkTopic . Sql.fromOnly)) (Sql.query_ (dbConn db) sql)

deleteTopic ::
  FirstAppDB ->
  Topic ->
  AppM ()
deleteTopic db topic =
  let sql = "DELETE FROM comments WHERE topic = ?"
   in runDB pure (Sql.execute (dbConn db) sql (Sql.Only (getTopic topic)))
-- Go to 'src/Level05/Core.hs' next.
