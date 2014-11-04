{-
overlapping :: Int -> [Char] -> ([Char], Int) -> Bool
overlapping myPos myName (name, pos) = pos == myPos && (not (name == myName))

thing :: [Char] ->               SF        (  (Int),  [([Char], Int)]  )               (   Maybe ([Char], Int),      ([Char], Int))
thing myName = proc(myPos, othrPos) -> do
    returnA -< ((find (overlapping myPos myName) othrPos), (myName, myPos))

s :: SF [Int] [Maybe ([Char], Int)]
s = combine [
           thing "A", 
           thing "B", 
           thing "C", 
           thing "D"]

events :: [(DTime, Maybe [()])]
events = 
        (1, Just empty):
        (3, Just empty):
        (5, Just empty):
        []

main = do
        mapM_ print $ embed (solarSystem) (empty, events)
-}

module BundleGroupSpec where

import Test.Hspec

spec :: Spec
spec = do
        return ()
