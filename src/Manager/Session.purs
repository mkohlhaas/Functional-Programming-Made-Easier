module Manager.Session where

import Prelude

import Data.JSDate (fromInstant, getTime)
import Data.Map (Map, filter, insert, update)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, genUUID)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar, put, take)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Now as Now
import Entity.Session (Session(..))
import Utils (withAVar)

type UserName = String
type AuthToken = UUID
type Sessions = Map AuthToken Session

startup :: Aff (AVar Sessions)
startup = AVar.new Map.empty

shutdown :: AVar Sessions -> Aff Unit
shutdown = void <<< take

sessionTimeout :: Number
sessionTimeout = 4.0 * 60.0 * 60.0 * 1000.0 -- 4 hours

now :: Aff Number
now = getTime <<< fromInstant <$> liftEffect Now.now

expireSessions :: AVar Sessions -> Aff Unit
expireSessions sessionsAVar = do
  sessions <- take sessionsAVar
  now' <- now
  let sessions' = filter (\(Session { lastTime }) -> now' - lastTime < sessionTimeout) sessions
  put sessions' sessionsAVar

verifySession :: AVar Sessions -> AuthToken -> Aff (Maybe Session)
verifySession sessionsAVar authToken = do
  expireSessions sessionsAVar
  withAVar sessionsAVar \sessions -> do
    now' <- now
    let
      newSessions = update (\(Session session) -> Just (Session $ session { lastTime = now' })) authToken sessions
      currentSession = Map.lookup authToken newSessions
    pure $ Tuple newSessions currentSession

createSession :: AVar Sessions -> UserName -> Aff AuthToken
createSession sessionsAVar userName = do
  lastTime <- now
  authToken <- liftEffect genUUID
  sessions <- take sessionsAVar
  let
    session = Session { authToken, userName, lastTime }
  put (insert authToken session sessions) sessionsAVar
  pure authToken

deleteSession :: AVar Sessions -> AuthToken -> Aff Unit
deleteSession sessionsAVar authToken = do
  sessions <- take sessionsAVar
  put (Map.delete authToken sessions) sessionsAVar