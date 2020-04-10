import Data.Vect

readVect : IO (len ** Vect len String)
readVect = do x <- getLine
              if (x == "")
                then pure (_ ** [])
                else (_ ** xs) <- readVect
                     pure (_ ** x :: xs)


zipInputs : IO ()
