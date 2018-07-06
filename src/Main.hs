-- dfa.hs
-- Author: Rodrigo Valle
--
-- Execute by running:
-- $ runhaskell dfa.hs

module Main where

import Data.NonEmpty.Set
import Data.Set (fromList)

type NonEmptySet = T

data State = Accepting
           | Rejecting
           deriving (Eq, Ord, Show)

type Transition = State -> Char -> State

data DFA stateType inputType = DFA
  { states :: T stateType
  , alphabet :: T inputType
  , transition :: stateType -> inputType -> stateType
  , start :: stateType
  , accept :: T stateType
  }

-- delta represents the transition function that counts an even number of 'a's
-- state 0 is the accepting state
delta :: Transition
delta Accepting 'a' = Rejecting -- count  'a'
delta Rejecting 'a' = Accepting -- count  'a'
delta currentState _ = currentState -- ignore 'b'

delta' :: State -> Char -> Maybe State
delta' Accepting 'a' = Just Rejecting
delta' Rejecting 'a' = Just Accepting
delta' currentState 'b' = Just currentState
delta' _ _ = Nothing


-- use the transitive closure of a DFA to execute
execute :: DFA stateType inputType ->  [inputType] -> stateType
execute d input = foldl (transition d) (start d) input

dfaAcceptsInput :: Ord stateType => DFA stateType inputType -> [inputType] -> Bool
dfaAcceptsInput d input = member (execute d input) (accept d)

-- some smoke tests
main = do
    let evenDFA = DFA { states     = insert Accepting $ Data.Set.fromList [Rejecting]
                      , alphabet   = insert 'a' $ Data.Set.fromList "b"
                      , transition = delta
                      , start      = Accepting
                      , accept     = insert Accepting $ Data.Set.fromList []
                      }

    print (execute evenDFA "aaaab")   -- returns Accepting
    print (execute evenDFA "aaaabba") -- returns Rejecting

    print (dfaAcceptsInput evenDFA "aaaab")
    print (dfaAcceptsInput evenDFA "aaaabba")
