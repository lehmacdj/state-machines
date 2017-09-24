module Data.Automata.DFA where

import Data.Automata.Moore
import Data.Automata.Classes

-- newtype DFA a s = DFA { getDFA :: (s -> Bool, Moore a s) }

-- -- given a transition function produce a DFA
-- mkDFA :: IsState s => (s -> a -> s) -> DFA a s
-- mkDFA = mkDFA' initial isFinal

-- -- generalized version of mkDFA that doesn't require an IsState constraint
-- mkDFA' :: s -> (s -> Bool) -> (s -> a -> s) -> DFA a s
-- mkDFA' initial isFinal delta = DFA (isFinal, mkMoore' initial delta)

-- -- given a DFA produce delta hat for that machine
-- runDFA :: DFA a s -> [a] -> Bool
-- runDFA (DFA (isFinal, mm)) = runMoore' isFinal mm
