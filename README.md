# Web server and client for my sites

## "Business" Feature wishlist

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
- [x] Configuration (Dhall)
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
- ### [Streamly for FRP](https://hackage.haskell.org/package/streamly)
  Actively maintained, Fastest streaming library, general purpose
- ### Polysemy (effect system?)
    [Polysemy](https://github.com/polysemy-research/polysemy) was written by Sandy McGuire,
    author of "Thinking with Types" and "Algebra Driven Design"

    [example Polysemy+Servant](https://github.com/EncodePanda/todo-rest)

    [Tutorial](https://haskell-explained.gitlab.io/blog/posts/2019/07/28/polysemy-is-cool-part-1/index.html)
- ### Other effect systems
  Effects are things like IO, random numbers, exceptions, mutable global state
  - [mtl](http://hackage.haskell.org/package/mtl) Basic, Super fast, O(n^2) instances, can't have more than one State, Reader etc. 
  - [freer-simple](http://hackage.haskell.org/package/freer-simple) Slow, no boilerplate, good type error messages
  - [fused effects](https://github.com/fused-effects/fused-effects) Super fast, lot of boilerplate
  - [Eff](https://github.com/hasura/eff) Very new, requires GHC changes not yet incoroporated
  - [Polysemy](https://github.com/polysemy-research/polysemy) Very fast, little boilerplate, good error messages


## Other useful libraries
[Artyom's Haskell toolbox](https://toolbox.brick.do//) Lists the author's favorite libraries for various tasks

## Concepts to learn better
- Type level programming. Read Mcguire book
-  McGuire also has a book Algebra Driven Design
- Effect systems
- FRP (use streamly?)

## References
[References](References.md)