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

data Command : Schema -> Type where
     Add : SchemaType schema -> Command schema
     Get : Integer -> Command schema
     Quit : Command schema

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

display : SchemaType schema -> String
-- SchemaType schema will evaluate for something like (Int, String),
-- so the definition will be (Int, String) -> String ?
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (l, r) = display l ++ ", " ++ display r

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let storeItems = items store in
                        case integerToFin pos (size store) of
                             Nothing => Just ("Out of range\n", store)
                             Just id => Just(display (index id (items store)) ++ "\n", store)
                             -- index id... returns (Int, String) for example,
                             -- that is not a schema, so how come display accept those params?


parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" str = Just (Add (?parseBySchema str))
parseCommand schema "get" val = case all isDigit (unpack val) of
                            False => Nothing
                            True => Just (Get (cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing


parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                 (cmd, args) => parseCommand schema cmd (ltrim args)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
                                Nothing => Just ("Invalid command\n", store)
                                Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                Just (Get pos) => getEntry pos store
                                Just Quit => Nothing


main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
