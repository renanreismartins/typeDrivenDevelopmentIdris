module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newItem = MkData schema _ (addToData items)
 where
   addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
   addToData [] = [newItem]
   addToData (x :: xs) = x :: addToData xs

{--
size : DataStore -> Nat
size (MkData size' items) = size'

items: (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'




data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

parseCommand : String -> String -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "size" "" = Just Size
parseCommand "search" val = Just (Search val)
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing


parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let storeItems = items store in
                         case integerToFin pos (size store) of
                              Nothing => Just ("Out of range\n", store)
                              Just id => Just (index id storeItems ++ "\n", store)

searchEntry : String -> Vect _ String -> Nat -> String
searchEntry term [] k = ""
searchEntry term (x :: xs) k = if Strings.isInfixOf term x
                                  then (show k ++ ": " ++ show x ++ "\n") ++ searchEntry term xs (S k)
                                  else searchEntry term xs (S k)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                                Nothing => Just ("Invalid command\n", store)
                                Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                Just (Get pos) => getEntry pos store
                                Just Size => Just ("Size " ++ show (size store) ++ "\n", store)
                                Just (Search str) => Just (searchEntry str (items store) 0, store)
                                Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
--}
