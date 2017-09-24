{-# LANGUAGE DeriveFunctor #-}

module Data.Automata.Moore where

import Data.Automata.Classes

-- this is just Cofree ((->) a) s
newtype Moore a s = Moore { getMoore :: (s, a -> Moore a s) }
    deriving (Functor)

-- given a transition function produce a Moore
mkMoore :: IsState s => (s -> a -> s) -> Moore a s
mkMoore = mkMoore' initial

-- generalized version of mkMoore that doesn't require an IsState constraint
mkMoore' :: s -> (s -> a -> s) -> Moore a s
mkMoore' initial delta = Moore (initial, transition initial)
    where
        transition s a =
            let s' = delta s a
             in Moore (s', transition s')

-- given a Moore produce delta hat
runMoore :: IsState s => Moore a s -> [a] -> Bool
runMoore = runMoore' isFinal

-- generalized version of runMoore that doesn't require an IsState constraint
runMoore' :: (s -> Bool) -> Moore a s -> [a] -> Bool
runMoore' isFinal (Moore (s, delta)) [] = isFinal s
runMoore' isFinal (Moore (s, delta)) (x:xs) = runMoore' isFinal (delta x) xs
