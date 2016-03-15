module Handler.Register where

import qualified Data.Map as M
import qualified Data.Text as T
import Import
import T3.Server

data RegisterResult
  = NameExists
  | NoName
  | Good UserName UserKey

getRegisterR :: Handler Html
getRegisterR = do
  (formWidget, formEnctype) <- generateFormPost sampleForm
  let submission = Nothing :: Maybe RegisterResult
  defaultLayout $ do
    setTitle "Register"
    $(widgetFile "register")

sampleForm :: Form Text
sampleForm = renderDivs $ areq textField "Name" Nothing

postRegisterR :: Handler Html
postRegisterR = do
  srv <- appServer <$> getYesod
  ((result, formWidget), formEnctype) <- runFormPost sampleForm
  submission <- case result of
    FormSuccess name -> do
      if T.null name
        then return $ Just NoName
        else do
          userKey <- liftIO genUserKey
          exists <- liftIO . atomically $ do
            users <- readTVar (srvUsers srv)
            if M.member name users
              then return True
              else writeTVar (srvUsers srv) (M.insert name userKey users) >> return False
          return $ Just (if exists then NameExists else Good name userKey)
    _ -> return Nothing
  defaultLayout $ do
    setTitle "Register"
    $(widgetFile "register")
