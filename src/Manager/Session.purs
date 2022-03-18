module Manager.Session where

import Prelude

import Data.JSDate (fromInstant, getTime)
import Data.Map (Map, filter, insert, update)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.UUID (UUID, genUUID)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar, empty, put, take)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Entity.Session (Session(..))

type Sessions = Map UUID Session

startup :: Aff (AVar Sessions)
startup = empty

shutdown :: AVar Sessions -> Aff Unit
shutdown = void <<< take

verifySession :: AVar Sessions -> UUID -> Aff (Maybe Session)
verifySession sessionsAVar authToken = do
  expireSessions sessionsAVar
  sessions <- take sessionsAVar
  now <- getTime <<< fromInstant <$> liftEffect now
  let newSessions = update (\(Session session) -> Just (Session $ session { lastTime = now })) authToken sessions
  put newSessions sessionsAVar
  pure $ Map.lookup authToken newSessions

createSession :: AVar Sessions -> String -> Aff UUID
createSession sessionsAVar userName = do
  lastTime <- getTime <<< fromInstant <$> liftEffect now
  authToken <- liftEffect genUUID
  sessions <- take sessionsAVar
  let
    session = Session { authToken, userName, lastTime }
  put (insert authToken session sessions) sessionsAVar
  pure authToken

sessionTimeout :: Number
sessionTimeout = 4.0 * 60.0 * 60.0 * 1000.0 -- 4 hours

expireSessions :: AVar Sessions -> Aff Unit
expireSessions sessionsAVar = do
  sessions <- take sessionsAVar
  now <- getTime <<< fromInstant <$> liftEffect now
  let sessions' = filter (\(Session { lastTime }) -> now - lastTime < sessionTimeout) sessions
  put sessions' sessionsAVar