{-# LANGUAGE DeriveFunctor #-}

module Data.StateMachine.MooreMachine where

import Data.StateMachine.Classes

-- this is just Cofree ((->) a) s
newtype MooreMachine a s = MooreMachine { getMM :: (s, a -> MooreMachine a s) }
    deriving (Functor)

-- given a transition function produce a MooreMachine
mkMooreMachine :: IsState s => (s -> a -> s) -> MooreMachine a s
mkMooreMachine = mkMooreMachine' initial

-- generalized version of mkMooreMachine that doesn't require an IsState constraint
mkMooreMachine' :: s -> (s -> a -> s) -> MooreMachine a s
mkMooreMachine' initial delta = MooreMachine (initial, transition initial)
    where
        transition s a =
            let s' = delta s a
             in MooreMachine (s', transition s')

-- given a MooreMachine produce delta hat
runMooreMachine :: IsState s => MooreMachine a s -> [a] -> Bool
runMooreMachine = runMooreMachine' isFinal

-- generalized version of runMooreMachine that doesn't require an IsState constraint
runMooreMachine' :: (s -> Bool) -> MooreMachine a s -> [a] -> Bool
runMooreMachine' isFinal (MooreMachine (s, delta)) [] = isFinal s
runMooreMachine' isFinal (MooreMachine (s, delta)) (x:xs) = runMooreMachine' isFinal (delta x) xs
