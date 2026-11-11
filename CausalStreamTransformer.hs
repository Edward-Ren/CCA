module CausalStreamTransformer where

import Prelude hiding (id, (.), init)
import Control.Category (Category(..))
import Control.Arrow (Arrow(..), ArrowLoop(..), (>>>), (***), second)
import CCAInitBasics (ArrowInit(..))

{- Why a Causal Stream Transformer? (Section 3, Figure 5) -}
{-
CCA laws describe stream computations abstractly.
Figure 5 gives a concrete implementation: SF.
Each step takes one input and returns an output plus a continuation.
The continuation allows the computation to remember state.
-}

{- 1. SF: a stream transformer -}
{-
SF a b transforms a stream of a values into a stream of b values.
unSF takes one input a and returns (current output b, next SF).
This definition is directly from Figure 5.
-}
newtype SF a b = SF { unSF :: a -> (b, SF a b) }

{- 2. Category instance (modern Haskell compatibility) -}
{-
The paper defines composition (>>>) in the Arrow instance.
Modern GHC requires Arrow to have a Category instance.
id leaves inputs unchanged; (g . f) runs f and then g.
-}
instance Category SF where
    id = SF step
      where
        step x = (x, SF step)

    g . f = SF (step f g)
      where
        step f0 g0 x =
            let (y, f1) = unSF f0 x
                (z, g1) = unSF g0 y
            in (z, SF (step f1 g1))

{- 3. Arrow instance (Figure 5) -}
{-
arr f applies a pure function to each input.
first f processes the first part of a pair only.
-}
instance Arrow SF where
    arr f = SF step
      where
        step x = (f x, SF step)

    first f = SF (step f)
      where
        step current (x, z) =
            let (y, next) = unSF current x
            in ((y, z), SF (step next))

{- 4. ArrowLoop instance (Figure 5) -}
{-
loop connects the second output to the second input (feedback).
It ties a lazy recursive knot. Not every loop is productive;
init gives the delay needed in the causal examples below.
-}
instance ArrowLoop SF where
    loop f = SF (step f)
      where
        step current x =
            let ((y, feedback), next) = unSF current (x, feedback)
            in (y, SF (step next))

{- 5. ArrowInit instance (Figure 5) -}
{-
The ArrowInit class is defined in CCAInitBasics.hs.
init i outputs i first; later it outputs the previous input.
It adds a one-step delay and stores state in the continuation.
-}
instance ArrowInit SF where
    init i = SF (step i)
      where
        step previous x = (previous, SF (step x))

{- 6. Run an SF on a stream (Figure 5) -}
{-
The paper defines runsf for nonempty streams.
We also handle [] so finite lists work.
-}
runsf :: SF a b -> [a] -> [b]
runsf _ [] = []
runsf current (x:xs) =
    let (y, next) = unSF current x
    in y : runsf next xs

{- Example 1: init / one-step delay -}
{- Input [5,8,12,20] gives output [0,5,8,12]. -}
exampleInit :: [Int]
exampleInit = runsf (init 0 :: SF Int Int) [5, 8, 12, 20]

{- Example 2: Commutativity Law (Figure 4) -}
{-
first f >>> second g = second g >>> first f
Two independent computations can be reordered.
-}
exampleComm1 :: [(Int, Int)]
exampleComm1 = runsf
    (first (arr (+1)) >>> second (arr (*2)) :: SF (Int, Int) (Int, Int))
    [(3, 4), (5, 6)]

exampleComm2 :: [(Int, Int)]
exampleComm2 = runsf
    (second (arr (*2)) >>> first (arr (+1)) :: SF (Int, Int) (Int, Int))
    [(3, 4), (5, 6)]

{- Example 3: Product Law (Figure 4) -}
{-
init i *** init j = init (i, j)
Two independent delays equal one delay on a pair.
-}
pairInputs :: [(Int, Int)]
pairInputs = [(1, 10), (2, 20), (3, 30)]

exampleProduct1 :: [(Int, Int)]
exampleProduct1 = runsf
    ((init 0 *** init 100) :: SF (Int, Int) (Int, Int))
    pairInputs

exampleProduct2 :: [(Int, Int)]
exampleProduct2 = runsf
    (init (0, 100) :: SF (Int, Int) (Int, Int))
    pairInputs

{- 7. Euler integration (Section 3, p. 472) -}
{-
The paper constructs an integral using arr, init, and loop.
Euler's rule: I_(n+1) = I_n + dt * x_n.
init 0 gives the initial accumulated value.
-}
dt :: Double
dt = 0.01

integral :: ArrowInit a => a Double Double
integral = loop (arr acc >>> init 0 >>> arr dup)
  where
    acc (x, i) = i + dt * x
    dup x = (x, x)

exampleIntegral :: [Double]
exampleIntegral = take 6 (runsf (integral :: SF Double Double) (repeat 1.0))

{- 8. Exponential example (pp. 471-473) -}
{-
The paper defines feedback through fixA:
e = 1 + integral e.
The samples approximate exp(t) at steps of dt = 0.01.
Use expSF to avoid a name clash with Prelude.exp.
-}
fixA :: ArrowLoop a => a b b -> a c b
fixA f = loop (second f >>> arr (dup . snd))
  where
    dup x = (x, x)

expSF :: SF () Double
expSF = fixA (integral >>> arr (+1))

exampleExp :: [Double]
exampleExp = take 6 (runsf expSF (repeat ()))

{- Run examples in GHCi: runExamples -}
runExamples :: IO ()
runExamples = do
    putStrLn "1. init (one-step delay)"
    print exampleInit

    putStrLn "\n2. Commutativity Law"
    print exampleComm1
    print exampleComm2
    print (exampleComm1 == exampleComm2)

    putStrLn "\n3. Product Law"
    print exampleProduct1
    print exampleProduct2
    print (exampleProduct1 == exampleProduct2)

    putStrLn "\n4. Euler Integration (constant input 1)"
    print exampleIntegral

    putStrLn "\n5. Exponential Function (paper example)"
    print exampleExp
