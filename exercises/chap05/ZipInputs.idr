import Data.Vect

readVect : IO (len ** Vect len String)
readVect = do x <- getLine
              if (x == "")
                 then pure (_ ** [])
                 else do (_ ** xs) <- readVect
                         pure (_ ** x :: xs)


zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector (blank line to end):"
               (len1 ** vect1) <- readVect
               putStrLn "Enter second vector (blank line to end):"
               (len2 ** vect2) <- readVect
               case exactLength len1 vect2 of
                    Nothing => putStrLn "Vectors are different lengths"
                    Just vect2' => putStrLn (show (zip vect1 vect2'))

readToBlank : IO (List String)
readToBlank = do line <- getLine
                 if (line == "")
                    then pure []
                    else do lines <- readToBlank
                            pure (line :: lines)




readAndSave : IO ()
readAndSave = do lines <- readToBlank
                 let content = unlines lines
                 fileName <- getLine
                 result <- (writeFile fileName content)
                 case result of
                   Left _ => putStrLn "Could not save the file"
                   Right _ => putStrLn "File saved"


myReadFile : File -> IO (n ** Vect n String)
myReadFile h = do Right line <- fGetLine h
                  isEOF <- (fEOF h)
                  if isEOF
                    then pure (_ ** [])
                    else do (_ ** xs) <- myReadFile h
                            pure (_ ** line :: xs)


readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right h <- openFile filename Read | pure (_ ** [])
                           result <- myReadFile h
                           _ <- closeFile h
                           pure result
