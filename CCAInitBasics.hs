
module CCAInitBasics where

import Control.Arrow
import Prelude hiding (init)

{- Why Causal Commutative Arrows (CCA)? -}
{-
Conventional Arrow and ArrowLoop laws are not strong enough
to capture the properties of stream computations.

CCA introduces:
1. Commutativity Law
2. ArrowInit and Product Law

These provide additional laws for reasoning about
causal, stateful computations and their optimization.
-}


{- 1. Commutativity Law -}
{-
first f >>> second g = second g >>> first f

Independent computations do not interfere with each other.
Their execution order can be exchanged.

This law is not guaranteed by conventional Arrow laws.
-}

exampleComm1 :: (Int, Int) -> (Int, Int)
exampleComm1 = first (+1) >>> second (*2)

exampleComm2 :: (Int, Int) -> (Int, Int)
exampleComm2 = second (*2) >>> first (+1)


{- 2. What is ArrowInit? -}
{-
ArrowInit extends ArrowLoop with an initialization operator.

class ArrowLoop a => ArrowInit a where
    init :: b -> a b b

init i:
- First output: initial value i
- Subsequent outputs: previous input

It introduces a one-step delay and stateful computation.

Causality:
The current output depends only on current and past inputs,
never on future inputs.
-}

class ArrowLoop a => ArrowInit a where
    init :: b -> a b b


{- 3. Product Law -}
{-
init i *** init j = init (i, j)

Two independent delays can be combined into one delay
over a pair of values.

This law is important for normalization and optimization.
-}


{- Run Examples -}

runExamples :: IO ()
runExamples = do
    putStrLn "Commutativity Law"
    print (exampleComm1 (3, 4))
    print (exampleComm2 (3, 4))
