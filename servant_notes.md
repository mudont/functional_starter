# Servant Notes

## Examples

### Simple with one endpoint
main -> app -> server -> API
```
data User = User {...}
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]
type MyAPI = "users" :> Get '[JSON] [User]
server :: Server MyAPI
server = return users
myAPI :: Proxy MyAPI
app :: Application
app = serve myAPI server
main = run 8081 app
```

### Simple with multiple endpoints

```
type MyAPI = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User

server :: Server MyAPI
server = return users
    :<|> return users[1]
myAPI :: Proxy MyAPI
app :: Application
app = serve myAPI server
main = run 8081 app         
```

## Types

### Servant types:

#### Server/ServerT
```
type Server api = ServerT api Handler
```

We can use a custom application monad instead of Handler
the handler methods will have to return `AppM ResultType` instead of `Handler ResultType`

We have to provide a *natural transformation* to convert `AppM` to `Handler`
Example to go from Reader to Handler:
```
readerToHandler :: Reader String a -> Handler a
readerToHandler r = return (runReader r "hi")
```

We will need `hoistServer` to create the application

```
type ReaderAPI = "a" :> Get '[JSON] Int
            :<|> "b" :> ReqBody '[JSON] Double :> Get '[JSON] Bool

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

readerServerT :: ServerT ReaderAPI (Reader String)
readerServerT = a :<|> b where
    a :: Reader String Int
    a = return 1797

    b :: Double -> Reader String Bool
    b _ = asks (== "hi")
    
readerToHandler :: Reader String a -> Handler a
readerToHandler r = return (runReader r "hi")

readerServer :: Server ReaderAPI
readerServer = hoistServer readerAPI readerToHandler readerServerT

app4 :: Application
app4 = serve readerAPI readerServer
```


#### serve
```
serve :: HasServer api '[] => Proxy api -> Server api -> Application
```

#### Handler

Handler uses the simplest type that lets handlers perform IO and return success or fail with throwError
```
newtype Handler a = Handler { runHandler' :: ExceptT ServerError IO a } deriving ...
```
Note that
```
ExceptT e (m :: * -> *) a  
constructor:  ExceptT (m (Either e a))

So runHandler' returns a
ExceptT (IO (Either ServerError a))

```

```
hoistServer :: HasServer api '[] => Proxy api -> (forall x. m x -> n x) -> ServerT api m -> ServerT api nSource

```