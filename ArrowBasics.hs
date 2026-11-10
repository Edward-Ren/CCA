
module ArrowBasics where

import Control.Arrow
import Control.Category ((>>>))


{- What is Arrow? -}
{-
An Arrow generlizes ordinary functions and Kleisli arrows.
It extends Category with operations for working on pairs.

Category provides:
    id :: a b b
    (>>>) :: a b c -> a c d -> a b d

Arrow provides:
class Control.Category.Category a => Arrow a where
    arr :: (b -> c) -> a b c
    first :: a b c -> a (b, d) (c, d)
    second :: a b c -> a (d, b) (d, c)
    (***) :: a b c -> a b' c' -> a (b, b') (c, c')
    (&&&) :: a b c -> a b c' -> a b (c, c')
-}


{- Example Functions -}
add1 :: Int -> Int
add1 x = x + 1

double :: Int -> Int
double x = x * 2


{- arr: lift a normal function into an Arrow -}
{- The combinator arr lifts a function from b to c
to a “pure” arrow computation from b to c,
namely, a b c where a is the arrow type.
The output of a pure arrow entirely depends on the input
(it is analogous to return in the Monad class). -}
-- arr :: (b -> c) -> a b c
exampleArr :: Int -> Int
exampleArr = arr add1


{- >>>: sequential composition, first execute f, then g -}
{- The operator composes two arrow computations
by connecting the output of the first to the input of the second
(and is analogous to bind >>= in the Monad class). -}
exampleSeq :: Int -> Int
exampleSeq = arr add1 >>> arr double


{- first: apply f to the first component of a pair -}
{- first converts an arrow computation taking one input
and one result, into an arrow computation,
taking two inputs and two results.
The original arrow is applied to the first part of the input,
and the result becomes the first part of the output.
The second part of the input is fed directly to the second part
of the output. -}
-- first :: a b c -> a (b, d) (c, d)
exampleFirst :: (Int, Int) -> (Int, Int)
exampleFirst = first (arr add1)


{- using the above three premises,we can define the following -}

{- second: apply f to the second component of a pair -}
-- second :: a b c -> a (d, b) (d, c)
-- second :: (Arrow a) => a b c -> a (d, b) (d, c)
-- second f = arr swap >>> first f >>> arr swap
-- where swap (a, b) = (b, a)
exampleSecond :: (Int, Int) -> (Int, Int)
exampleSecond = second (arr double)


{- (***): parallel composition on seperate inputs -}
-- (***) :: a b c -> a b' c' -> a (b, b') (c, c')
-- (***) :: (Arrow a) => a b c -> a b' c' -> a (b, b') (c, c')
-- f *** g = first f >>> second g
exampleParallel :: (Int, Int) -> (Int, Int)
exampleParallel = arr add1 *** arr double


{- (&&&): apply both Arrows to the same input -}
-- (&&&) :: a b c -> a b c' -> a b (c, c')
-- (&&&) :: (Arrow a) => a b c -> a b c' -> a b (c, c')
-- f &&& g = arr (\x -> (x, x)) >>> first f >>> arr swap >>> first g >>> arr swap
exampleFanout :: Int -> (Int, Int)
exampleFanout = arr add1 &&& arr double




{- Run all examples -}
runExamples :: IO ()
runExamples = do
    print (exampleArr 3) -- 4
    print (exampleSeq 3) -- 8
    print (exampleFirst (3, 5)) -- (4, 5)
    print (exampleSecond (3, 5)) -- (3, 10)
    print (exampleParallel (3, 5)) -- (4, 10)
    print (exampleFanout 3) -- (4, 6)