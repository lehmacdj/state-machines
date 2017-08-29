{-# LANGUAGE DeriveFunctor #-}

module Data.StateMachine.DFA where

import Data.StateMachine.Classes

-- this is just Cofree ((->) a) s
newtype DFA a s = DFA { getDFA :: (s, a -> DFA a s) }
    deriving (Functor)

-- given a transition function produce a DFA
mkDFA :: IsState s => (s -> a -> s) -> DFA a s
mkDFA = mkDFA' initial

-- generalized version of mkDFA that doesn't require an IsState constraint
mkDFA' :: s -> (s -> a -> s) -> DFA a s
mkDFA' initial delta = DFA (initial, transition initial)
    where
        transition s a =
            let s' = delta s a
             in DFA (s', transition s')

-- given a DFA produce delta hat
runDFA :: IsState s => DFA a s -> [a] -> Bool
runDFA = runDFA' isFinal

-- generalized version of runDFA that doesn't require an IsState constraint
runDFA' :: (s -> Bool) -> DFA a s -> [a] -> Bool
runDFA' isFinal (DFA (s, delta)) [] = isFinal s
runDFA' isFinal (DFA (s, delta)) (x:xs) = runDFA' isFinal (delta x) xs
