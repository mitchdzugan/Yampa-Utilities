{-# LANGUAGE Arrows #-}

module BundleGroup (bundleGroup) where

import FRP.Yampa
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Data.List

poolIntercomm :: (Int, SF (a, [b]) (c, b)) -> SF ([a], [b]) [b] -> SF ([a], [b]) [b]
poolIntercomm (ind, cur) acc = proc(inputs, _) -> do
    rec blist <- acc -< (inputs, blist)
        (_, b) <- cur -< ((inputs !! ind), blist) 
    returnA -< b:blist


routeIntercomm :: SF ([a], [b]) [b] -> (Int, SF (a, [b]) (c, b)) -> SF [a] c
routeIntercomm folded (ind, cur) = proc(inputs) -> do
    rec blist <- folded -< (inputs, blist)
        (c, _) <- cur -< ((inputs !! ind), blist)
    returnA -< c

poolOutput :: SF [a] c -> SF [a] [c] -> SF [a] [c]
poolOutput cur acc = proc(inputs) -> do
    c <- cur -< inputs
    cl <- acc -< inputs
    returnA -< c:cl

bundleGroup :: [SF (a, [b]) (c, b)] -> SF [a] [c]
bundleGroup signals = bundled
    where signalsWithInd = zip [0..((length signals)-1)] signals
          intercomm = foldr poolIntercomm (arrPrim (\_ -> [])) signalsWithInd
          communicating = map (routeIntercomm intercomm) signalsWithInd
          bundled = foldr poolOutput (arrPrim (\_ -> [])) communicating

