# Web server and client for my sites

## Feature wishlist

- Betting/Trading exchange
- Tennis
    * club page
    * matches/leagues
    * scheduling winter contracts
- Cricket
## Infrastructural features

- [ ] [optparse-generic](https://hackage.haskell.org/package/optparse-generic-1.4.4/docs/Options-Generic.html)
- [ ] Logging
- [ ] [Lucid for HTML](https://hackage.haskell.org/package/lucid)
- [x] Configuration
- [ ] Websockets
- [ ] OpenID Connect

      ~/proj/hs/servant-oidc
      This just authenticates
      Needs to set cookies/JWT and use that in subsequent
      requests to check identity


## Tech stack
- Authentication: OpenId Connect Google/Microsoft
- Communication : REST + Websockets
- FRP: streamly seems to be a new lib (Claims to be close to Rx)
- Client options: 
  * Purescript + Halogen + REST+Websocket + Nice default styling
  * Elm
  * GHCJS Reflex-DOM
  * GHCJS Miso (Not FRP)
  * ReScript
- Servant + Selda + Websocket server + Postgres
- [Streamly for FRP](https://hackage.haskell.org/package/streamly)


## [Other useful libraries](https://toolbox.brick.do//)