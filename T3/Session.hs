module T3.Session
  ( runSession
  , UserInit
  , Callback
  ) where

import Prelude
import T3.Game
import T3.Comm.Types
import T3.Comm.Class
import T3.Comm.Game

import Control.Monad.State.Strict

type Callback = Board -> IO ()

data SessionData = SessionData
  { sessReq :: XO -> IO (Loc, Callback)
  , sessRespX :: Callback
  , sessRespO :: Callback
  , sessLog :: Win XO -> Lose XO -> Board -> IO ()
  , sessBoard :: Board
  }

newtype Session a = Session { unSession :: StateT SessionData IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState SessionData)

type UserInit = (UserId, Callback, IO (Loc, Callback))

runSession
    :: UserInit
    -> UserInit
    -> (Win UserId -> Lose UserId -> Board -> IO ())
    -> IO ()
runSession (xUI, xCB, xReq) (oUI, oCB, oReq) logger = let
  req X = xReq
  req O = oReq
  cb X = xCB
  cb O = oCB
  ui X = xUI
  ui O = oUI
  b = emptyBoard
  sessDat = SessionData req (cb X) (cb O) (\w l -> logger (fmap ui w) (fmap ui l)) b
  in evalStateT (unSession $ run b) sessDat

instance Comm Session where
  sendGameState xo = do
    s <- get
    liftIO $ (respXO xo s) (sessBoard s)
  recvMove xo = do
    req <- gets (flip sessReq xo)
    (loc, resp) <- liftIO req
    updateResp resp
    return loc
    where
      updateResp resp = do
        sess <- get
        put $ case xo of
          X -> sess { sessRespX = resp }
          O -> sess { sessRespO = resp }
  sendFinal xo final = do
    s <- get
    liftIO $ (respXO xo s) (sessBoard s)
  tally w l = do
    s <- get
    liftIO $ (sessLog s) w l (sessBoard s)
  updateBoard b = do
    sess <- get
    put $ sess { sessBoard = b }

respXO :: XO -> SessionData -> Callback
respXO X = sessRespX
respXO O = sessRespO

instance Game Session  where
  move = move'
  forfeit = forfeit'
  end = end'
  tie = tie'
  step = step'
