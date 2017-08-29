{-# LANGUAGE DeriveFunctor #-}

module Data.StateMachine.NFA where

import Data.StateMachine.Classes

-- this is just Cofree ((->) a) [s]
newtype NFA a s = NFA { getNFA :: ([s], a -> NFA a s) }
    deriving (Functor)

-- given a transition function produce a NFA
mkNFA :: IsState s => (s -> a -> [s]) -> NFA a s
mkNFA = mkNFA' initial

-- generalized version of mkNFA that doesn't require an IsState constraint
mkNFA' :: s -> (s -> a -> [s]) -> NFA a s
mkNFA' initial delta = NFA ([initial], transition [initial])
    where
        transition ss a =
            let ss' = concat [delta s a | s <- ss]
             in NFA (ss', transition ss')

-- given a NFA produce delta hat
runNFA :: IsState s => NFA a s -> [a] -> Bool
runNFA = runNFA' isFinal

-- generalized version of runNFA that doesn't require an IsState constraint
runNFA' :: (s -> Bool) -> NFA a s -> [a] -> Bool
runNFA' isFinal (NFA (ss, delta)) [] = any isFinal ss
runNFA' isFinal (NFA (ss, delta)) (x:xs) = runNFA' isFinal (delta x) xs
