module ArrowLoopBasics where

import Control.Arrow

{- What is ArrowLoop? -}
{-
ArrowLoop extends Arrow with feedback.

class Arrow a => ArrowLoop a where
    loop :: a (b, d) (c, d) -> a b c

b: external input
c: external output
d: feedback value

The second output d is fed back as the second input d.
-}

{- Example 1 -}
exampleLoop1 :: Int -> Int
exampleLoop1 = loop (arr (\(x, d) -> (x + d, 2)))

{- Example 2-}
exampleLoop2 :: Int -> Int
exampleLoop2 = loop (arr (\(x, d) -> (x * d, x + 1)))



runExamples :: IO ()
runExamples = do
    print (exampleLoop1 3)
    print (exampleLoop2 3)