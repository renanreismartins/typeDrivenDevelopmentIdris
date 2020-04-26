module Main

import Data.Vect


infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

data Command : Schema -> Type where
     SetSchema : (newSchema : Schema) -> Command schema
     Add : SchemaType schema -> Command schema
     Get : Maybe Integer -> Command schema
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
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (x .+. y)} (l, r) = display l ++ ", " ++ display r

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let storeItems = items store in
                        case integerToFin pos (size store) of
                             Nothing => Just ("Out of range\n", store)
                             Just id => Just(display (index id (items store)) ++ "\n", store)


setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              S k => Nothing


parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs)
    = case xs of
           [] => Just SString
           _ => do parsedSchema <- parseSchema xs
                   Just (SString .+. parsedSchema)
parseSchema ("Int" :: xs)
    = case xs of
           [] => Just SInt
           _ => do parsedSchema <- parseSchema xs
                   Just (SInt .+. parsedSchema)
parseSchema ("Char" :: xs)
   = case xs of
          [] => Just SChar
          _ => do parsedSchema <- parseSchema xs
                  Just (SChar .+. parsedSchema)
parseSchema _ = Nothing


parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString item = getQuoted (unpack item)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                                 (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                 _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt item = case span isDigit item of
                             ("", rest) => Nothing
                             (num, rest) => Just (cast num, ltrim rest)
parsePrefix SChar item = case unpack item of
                              (x :: xs) => Just (x, ltrim (pack xs))
                              [] => Nothing
parsePrefix (schemal .+. schemar) item = do (lVal, item') <-parsePrefix schemal item
                                            (rVal, item'') <- parsePrefix schemar item'
                                            Just ((lVal, rVal), item'')


parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _ => Nothing
                                  _ => Nothing


parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" str = do parsedStr <- parseBySchema schema str
                                   Just (Add parsedStr)
parseCommand schema "get" "" = Just (Get Nothing)
parseCommand schema "get" val = case all isDigit (unpack val) of
                                     False => Nothing
                                     True => Just (Get (Just (cast val)))
parseCommand schema "quit" "" = Just Quit
parseCommand schema "schema" rest = do schemaOk <- parseSchema (words rest)
                                       Just (SetSchema schemaOk)
parseCommand _ _ _ = Nothing


parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                 (cmd, args) => parseCommand schema cmd (ltrim args)


displayAll : Nat -> Vect size (SchemaType schema) -> String
displayAll k [] = ""
displayAll k (x :: xs) = let rest = displayAll (S k) xs in
                             show k ++ ": " ++ display x ++ "\n" ++ rest

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
                                Nothing => Just ("Invalid command\n", store)
                                Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                Just (Get (Just pos)) => getEntry pos store
                                Just (Get Nothing) => Just (displayAll 0 (items store), store)
                                Just Quit => Nothing
                                Just (SetSchema schema') => case setSchema store schema' of
                                                                 Nothing => Just ("Can't update store schema\n", store)
                                                                 Just store' => Just ("Ok\n", store')


main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
