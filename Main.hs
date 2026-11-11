module Main where

import FRP.Yampa
import FRPIntegral

main :: IO ()
main = do
    let dt = 0.1
    let results = embed expSF
            ((), replicate 5 (dt, Nothing))
    print results