{-# LANGUAGE OverloadedStrings #-}

module Util.Email where

import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Network.Mail.Mime as Mime
import Network.Mail.SMTP
import Prelude

from :: Address
from = Address Nothing "murali@mariandrive.com"

makeRecipient :: Text -> [Address]
makeRecipient t = [Address (Just t) t]

cc :: [Address]
cc = []

bcc :: [Address]
bcc = []

body :: Lazy.Text -> Mime.Part
body = Mime.plainPart

html :: Mime.Part
html = Mime.htmlPart "<h1>HTML</h1>"

mail :: Text -> Text -> Lazy.Text -> IO ()
mail to subject b = do
  print $ "Sending email to " <> to <> " about " <> subject
  renderSendMail $ simpleMail from (makeRecipient to) cc bcc subject [body b]
