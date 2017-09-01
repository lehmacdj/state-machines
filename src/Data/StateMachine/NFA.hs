module Data.StateMachine.NFA where

import Data.StateMachine.MooreMachine
import Data.StateMachine.Classes

-- this is just Cofree ((->) a) s
newtype NFA a s = NFA { getNFA :: (s -> Bool, MooreMachine a [s]) }

-- given a transition function produce a NFA
mkNFA :: IsState s => (s -> a -> [s]) -> NFA a s
mkNFA = mkNFA' initial isFinal

-- generalized version of mkNFA that doesn't require an IsState constraint
mkNFA' :: s -> (s -> Bool) -> (s -> a -> [s]) -> NFA a s
mkNFA' initial isFinal delta = NFA (isFinal, mkMooreMachine' [initial] delta')
    where delta' ss a = concat [ delta s a | s <- ss ]

-- given a NFA produce delta hat for that machine
runNFA :: NFA a s -> [a] -> Bool
runNFA (NFA (isFinal, mm)) = runMooreMachine' isFinal' mm
    where isFinal' = any isFinal
