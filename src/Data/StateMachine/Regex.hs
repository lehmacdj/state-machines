module Data.StateMachine.Regex where

import Data.StateMachine.NFA

import Control.Monad.State

data Regex a = Epsilon | Void | Literal a
             | Star (Regex a)
             | Concat (Regex a) (Regex a)
             | Or (Regex a) (Regex a)

type RS = State Integer
type RegexState = Integer

getNext :: RS RegexState
getNext = get <* modify succ

-- wow translating regex to FSMs is actually a lot more complicated than I would
-- have thought initially
-- we need to think about how we will generate states. Ideally we use some
-- mathematical formula to relate states. This means we don't need to use a
-- state monad such as RS

-- mkNFADelta :: Regex -> RS (s -> a -> [s])
-- mkNFADelta Void = pure $ \s a -> []
-- mkNFADelta Epsilon = pure $ \s a -> []
-- mkNFADelta _ = undefined

-- toNFA :: Regex -> NFA Bit RegexState
-- toNFA = mkNFA' 0 . fst . (`runState` 0) . mkNFADelta
