let port
    : Integer
    = +8080

in  { port
    , dbHost = "localhost"
    , dbPort = 5432
    , database = "tennis"
    , schema = None Text
    , dbUsername = Some "murali"
    , dbPassword = None Text
    , redirectUri = "/google/cb"
    , clientId =
        "500140516683-3avj9ie1bc9oh4jii07kmvl3v5i3vcd7.apps.googleusercontent.com"
    , clientPassword = "4HIZfO7UUAoEUAFGmlq3pod_"
    , jwkFile = "jwk.dat"
    }
