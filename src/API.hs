{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Models
import Servant (Get, JSON, Raw, type (:<|>), type (:>))

type API =
  "people" :> Get '[JSON] [Person]
    :<|> "groups" :> Get '[JSON] [Group]
    :<|> "static" :> Raw