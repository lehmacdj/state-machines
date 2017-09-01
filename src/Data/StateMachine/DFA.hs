module Data.StateMachine.DFA where

import Data.StateMachine.MooreMachine
import Data.StateMachine.Classes

-- this is just Cofree ((->) a) s
newtype DFA a s = DFA { getDFA :: (s -> Bool, MooreMachine a s) }

-- given a transition function produce a DFA
mkDFA :: IsState s => (s -> a -> s) -> DFA a s
mkDFA = mkDFA' initial isFinal

-- generalized version of mkDFA that doesn't require an IsState constraint
mkDFA' :: s -> (s -> Bool) -> (s -> a -> s) -> DFA a s
mkDFA' initial isFinal delta = DFA (isFinal, mkMooreMachine' initial delta)

-- given a DFA produce delta hat for that machine
runDFA :: DFA a s -> [a] -> Bool
runDFA (DFA (isFinal, mm)) = runMooreMachine' isFinal mm
