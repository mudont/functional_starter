{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Queries where

-- BEgin paste WilliamYao's selda code
import ClassyPrelude hiding (group)
import Data.Fixed (Pico)
import Data.Time
import Database.Selda hiding (Group)
import Database.Selda.PostgreSQL
import Models

mkUTCTime ::
  (Integer, Int, Int) ->
  (Int, Int, Pico) ->
  UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime
    (fromGregorian year mon day)
    (timeOfDayToTime (TimeOfDay hour min sec))

--------------------
-- QUERIES
--------------------

allPeople :: Query s (Row s Person)
allPeople = select person

allGroups :: Query s (Row s Group)
allGroups = select group

-- |
-- Marks that haven't been erased yet.
-- activeMarks :: Query s (Row s Mark)
-- activeMarks = do
--   mark <- select marks
--   erasedMark <-
--     leftJoin
--       (\em -> em ! #erasedMarkMarkID .== mark ! #markID)
--       (select erasedMarks)
--   restrict (isNull (erasedMark ? #erasedMarkMarkID))
--   pure mark

-- activelyPursuingMarks :: Query s (Row s Hitman)
-- activelyPursuingMarks = distinct $ do
--   hitman <- select hitmen
--   pursuingMark <-
--     innerJoin
--       (\pm -> pm ! #pursuingMarkHitmanID .== hitman ! #hitmanID)
--       (select pursuingMarks)
--   active <- activeMarks
--   restrict (pursuingMark ! #pursuingMarkMarkID .== active ! #markID)
--   pure hitman

-- erasedSince :: UTCTime -> Query s (Row s Mark)
-- erasedSince since = do
--   mark <- select marks
--   erasedMark <-
--     innerJoin
--       (\em -> em ! #erasedMarkMarkID .== mark ! #markID)
--       (select erasedMarks)
--   restrict (erasedMark ! #erasedMarkCreatedAt .> literal since)
--   pure mark

-- erasedSinceBy :: UTCTime -> ID Hitman -> Query s (Row s Mark)
-- erasedSinceBy since hid = do
--   erased <- erasedSince since
--   erasedMark <-
--     innerJoin
--       (\em -> em ! #erasedMarkMarkID .== erased ! #markID)
--       (select erasedMarks)
--   hitman <-
--     innerJoin
--       (\h -> erasedMark ! #erasedMarkHitmanID .== h ! #hitmanID)
--       (select hitmen)
--   restrict (hitman ! #hitmanID .== literal hid)
--   pure erased

-- totalBountiesAwarded :: Query s (Row s Hitman :*: Col s Int)
-- totalBountiesAwarded = do
--   (hitmanID :*: totalBounty) <- aggregate $ do
--     hitman <- select hitmen
--     erasedMark <-
--       leftJoin
--         (\em -> em ! #erasedMarkHitmanID .== hitman ! #hitmanID)
--         (select erasedMarks)
--     hitmanID <- groupBy (hitman ! #hitmanID)
--     pure
--       ( hitmanID
--           :*: sum_
--             ( ifNull
--                 (literal 0)
--                 (erasedMark ? #erasedMarkAwardedBounty)
--             )
--       )
--   hitman <-
--     innerJoin
--       (\h -> h ! #hitmanID .== hitmanID)
--       (select hitmen)
--   pure (hitman :*: totalBounty)

-- totalBountyAwarded :: ID Hitman -> Query s (Col s Int)
-- totalBountyAwarded hid = do
--   (hitman :*: bounty) <- totalBountiesAwarded
--   restrict (hitman ! #hitmanID .== literal hid)
--   pure bounty

-- latestHits :: Query s (Row s Hitman :*: Row s (Maybe Mark))
-- latestHits = do
--   hitman <- select hitmen
--   (_ :*: eachDate :*: minID) <-
--     leftJoin
--       (\(hid :*: _) -> hid .== hitman ! #hitmanID)
--       minByDate
--   (_ :*: maxDate) <-
--     leftJoin
--       (\(hid :*: _) -> hid .== hitman ! #hitmanID)
--       maxDate
--   mark <- leftJoin (\m -> just (m ! #markID) .== minID) (select marks)
--   restrict (maxDate .== eachDate .|| isNull maxDate)
--   pure (hitman :*: mark)
--   where
--     minByDate :: Query s (Col s (ID Hitman) :*: Col s UTCTime :*: Col s (Maybe (ID Mark)))
--     minByDate = aggregate $ do
--       hitman <- select hitmen
--       erasedMark <-
--         innerJoin
--           (\em -> em ! #erasedMarkHitmanID .== hitman ! #hitmanID)
--           (select erasedMarks)
--       hid <- groupBy (hitman ! #hitmanID)
--       date <- groupBy (erasedMark ! #erasedMarkCreatedAt)
--       pure (hid :*: date :*: min_ (erasedMark ! #erasedMarkMarkID))

--     maxDate :: Query s (Col s (ID Hitman) :*: Col s (Maybe UTCTime))
--     maxDate = aggregate $ do
--       hitman' <- select hitmen
--       erasedMark <-
--         innerJoin
--           (\em -> em ! #erasedMarkHitmanID .== hitman' ! #hitmanID)
--           (select erasedMarks)
--       hid <- groupBy (hitman' ! #hitmanID)
--       pure (hid :*: max_ (erasedMark ! #erasedMarkCreatedAt))

-- latestHit :: ID Hitman -> Query s (Row s (Maybe Mark))
-- latestHit hid = do
--   (hitman :*: mmark) <- latestHits
--   restrict (hitman ! #hitmanID .== literal hid)
--   pure mmark

-- singularPursuer :: Query s (Row s Hitman :*: Row s Mark)
-- singularPursuer = do
--   active <- activeMarks
--   (_ :*: numPursuers) <-
--     innerJoin
--       (\(mid :*: _) -> mid .== active ! #markID)
--       pursuerCounts
--   pursuingMark <-
--     innerJoin
--       (\em -> em ! #pursuingMarkMarkID .== active ! #markID)
--       (select pursuingMarks)
--   hitman <-
--     innerJoin
--       (\h -> h ! #hitmanID .== pursuingMark ! #pursuingMarkHitmanID)
--       (select hitmen)
--   restrict (numPursuers .== literal 1)
--   pure (hitman :*: active)
--   where
--     pursuerCounts :: Query s (Col s (ID Mark) :*: Col s Int)
--     pursuerCounts = aggregate $ do
--       pursuingMark <- select pursuingMarks
--       mid <- groupBy (pursuingMark ! #pursuingMarkMarkID)
--       pure (mid :*: count (pursuingMark ! #pursuingMarkHitmanID))

-- marksOfOpportunity :: Query s (Row s Hitman :*: Row s Mark)
-- marksOfOpportunity = do
--   erasedMark <- select erasedMarks
--   hitman <-
--     innerJoin
--       (\h -> h ! #hitmanID .== erasedMark ! #erasedMarkHitmanID)
--       (select hitmen)
--   mark <-
--     innerJoin
--       (\m -> m ! #markID .== erasedMark ! #erasedMarkMarkID)
--       (select marks)
--   pursuingMark <-
--     leftJoin
--       ( \pm ->
--           pm ! #pursuingMarkHitmanID .== erasedMark ! #erasedMarkHitmanID
--             .&& pm ! #pursuingMarkMarkID .== erasedMark ! #erasedMarkMarkID
--       )
--       (select pursuingMarks)
--   restrict (isNull (pursuingMark ? #pursuingMarkMarkID))
--   pure (hitman :*: mark)

-- --------------------
-- -- INSERTS/UPDATES
-- --------------------

-- insertSeedHandlers :: SeldaM PG ()
-- insertSeedHandlers =
--   insert_
--     handlers
--     [ Handler def "Olive" def def,
--       Handler def "Pallas" def def
--     ]

-- insertSeedHitmen :: SeldaM PG ()
-- insertSeedHitmen =
--   insert_
--     hitmen
--     [ Hitman def "Callaird" (toId 1) def def,
--       Hitman def "Bomois" (toId 1) def def,
--       Hitman def "Dune" (toId 1) def def
--     ]

-- insertSeedMarks :: SeldaM PG ()
-- insertSeedMarks =
--   insert_
--     marks
--     [ Mark def 25000 "John" "Tosti" Nothing def def,
--       Mark def 50000 "Macie" "Jordan" Nothing def def,
--       Mark def 33000 "Sal" "Aspot" Nothing def def,
--       Mark def 10000 "Lars" "Andersen" Nothing def def
--     ]

-- insertSeedPursuingMarks :: SeldaM PG ()
-- insertSeedPursuingMarks =
--   insert_
--     pursuingMarks
--     [ PursuingMark (toId 1) (toId 2) (mkUTCTime (2018, 7, 1) (0, 0, 0)) def,
--       PursuingMark (toId 2) (toId 2) (mkUTCTime (2018, 7, 2) (0, 0, 0)) def,
--       PursuingMark (toId 2) (toId 4) (mkUTCTime (2019, 5, 5) (0, 0, 0)) def,
--       PursuingMark (toId 3) (toId 3) (mkUTCTime (2018, 5, 13) (0, 0, 0)) def,
--       PursuingMark (toId 3) (toId 2) (mkUTCTime (2019, 2, 15) (0, 0, 0)) def
--     ]

-- insertSeedErasedMarks :: SeldaM PG ()
-- insertSeedErasedMarks =
--   insert_
--     erasedMarks
--     [ ErasedMark (toId 1) (toId 2) 30000 (mkUTCTime (2018, 9, 3) (0, 0, 0)) def,
--       ErasedMark (toId 1) (toId 1) 55000 (mkUTCTime (2019, 2, 2) (0, 0, 0)) def,
--       ErasedMark (toId 3) (toId 3) 27000 (mkUTCTime (2018, 6, 30) (0, 0, 0)) def
--     ]

-- increaseListBounty :: Int -> ID Mark -> SeldaM PG ()
-- increaseListBounty amt mid =
--   update_
--     marks
--     (\m -> m ! #markID .== literal mid)
--     (\m -> with m [#markListBounty += literal amt])

-- End paste WilliamYao's selda code
