module Data.StateMachine.Regex.Binary where

import Data.StateMachine.NFA

import Control.Monad.State

data Bit = I | O

data Regex = Epsilon | Void | Literal Bit
           | Star Regex | Concat Regex Regex | Or Regex Regex

type RS = State Integer
type RegexState = Integer

getNext :: RS RegexState
getNext = get <* modify succ

-- wow translating regex to FSMs is actually a lot more complicated than I would
-- have thought initially

-- mkNFADelta :: Regex -> RS (s -> a -> [s])
-- mkNFADelta Void = pure $ \s a -> []
-- mkNFADelta Epsilon = pure $ \s a -> []
-- mkNFADelta _ = undefined

-- toNFA :: Regex -> NFA Bit RegexState
-- toNFA = mkNFA' 0 . fst . (`runState` 0) . mkNFADelta
