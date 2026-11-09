{-# LANGUAGE Arrows #-}

module FRPIntegral where

import FRP.Yampa

expSF :: SF () Double
expSF = proc () -> do
    rec
        let e = 1 + i
        i <- integral -< e
    returnA -< e