module Handler.RegisterPage where

import Import
import Handler.Instance ()
import T3.Web
import T3.Server (UserKey(..), UserName(..))

getRegisterPageR :: Handler Html
getRegisterPageR = do
  (formWidget, formEnctype) <- generateFormPost sampleForm
  let submission = Nothing :: Maybe RegisterResult
  defaultLayout $ do
    setTitle "Register"
    $(widgetFile "register")

sampleForm :: Form Text
sampleForm = renderDivs $ areq textField "Name" Nothing

postRegisterPageR :: Handler Html
postRegisterPageR = do
  ((result, formWidget), formEnctype) <- runFormPost sampleForm
  submission <- case result of
    FormSuccess name -> register (Just $ UserName name)
    _ -> return Nothing
  defaultLayout $ do
    setTitle "Register"
    $(widgetFile "register")
