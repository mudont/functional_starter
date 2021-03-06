1. [ ] Merge in from other repos
    - [X] `servant-oidc` has **elm client** (with google-auth+servant)
    - [ ] `oplog-broadcast` has **websocket** push (+ auth in servant)
2. Add Forgot Password screen
   - Reset password button
   - Server creates a reset link with a random token
      + saves it to pending reset table with a timestamp
      + API to handle reset with a token. Basically a login link with special token. It should login the user if the token is good. After each successful login, clear pending resets for the user

3. Registration needs Captcha, second Password
4. use [https://hackage.haskell.org/package/relude](Relude) instead of other Preludes
5. Profile GET/POST
6. Events: Show, signup
7. Player info
8. Ashok's list: Few things that would really be useful are 
   1) Adding new user & reset passwords
   2) Hacker's league match up &  notification (homepage/email/whatsup) - This is more of an automation on the BE
   3) Scores input - both hackers & usta league 
   4) capture usta opponents/match notes.
   5) leaderboard
   
9. [ ] Use this Elm architecture [simple example](https://github.com/halfzebra/elm-examples.git) or the [Full SPA](https://github.com/halfzebra/elm-spa-example)
10. [x] Servant REST API framework
11. [x] PostgreSQL
12. [x] Selda PostgreSQL client
13. [X] OIDC auth
14. [ ] MIgrate database. [opaleye-gen](https://github.com/folsen/opaleye-gen) might be useful\
   Couldn't find a generator for Selda. But Selda doesn't support all the PG column types anyway.
     
11. [X] `DjangoPassword` lib for verifying and hashing Django compatible passwords
12. [ ] Write cli script to convert CMHackers database to new database. 
13. [ ] [optparse-generic](https://hackage.haskell.org/package/optparse-generic-1.4.4/docs/Options-Generic.html)
14. [ ] Logging
15. [ ] [Lucid for HTML](https://hackage.haskell.org/package/lucid)
16. [x] Configuration (Dhall)
17. [ ] Merge in from other repos
   - [X] `servant-oidc` has **elm client** (with google-auth+servant)
   - [ ] `oplog-broadcast` has **websocket** push (+ auth in servant)
13. [ ] Organize and clean up source code
14. [ ] Good effects library
15. [ ] Use Kafka
## Applications
1. [Separate private Repo](https://github.com/mudont/project_ideas.git)