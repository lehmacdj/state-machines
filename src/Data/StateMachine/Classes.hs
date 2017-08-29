{-# LANGUAGE TypeFamilies #-}
module Data.StateMachine.Classes where

class IsState s where
    isFinal :: s -> Bool
    initial :: s

-- not a particularly useful class, mostly just here for interest right now
class StateMachine sm where
    type Alphabet sm
    type States sm
    accepts :: sm -> [Alphabet sm] -> Bool
