module main

import Data.Vect

main : IO ()
main = ?main_rhs


data DataStore : Type where
     MkData : (size : Nat) ->
              (item : Vect size String) ->
              DataStore

size : DataStore -> Nat
size (MkData size' items) = size'

items: (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs
