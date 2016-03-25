module Handler.Register where

import Import
import Handler.Instance ()
import T3.Web
import T3.Server (UserKey(..), UserName(..))

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
  ((result, formWidget), formEnctype) <- runFormPost sampleForm
  submission <- case result of
    FormSuccess name -> fmap Just (register (UserName name))
    _ -> return Nothing
  defaultLayout $ do
    setTitle "Register"
    $(widgetFile "register")
