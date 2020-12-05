{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Models
import Servant (Get, JSON, Raw, type (:<|>), type (:>))

type API =
  "marks" :> Get '[JSON] [Mark]
    :<|> "handlers" :> Get '[JSON] [Handler]
    :<|> "static" :> Raw