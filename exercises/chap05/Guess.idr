readNumber : IO (Maybe Nat)
readNumber = do input <- getLine
                if all isDigit (unpack input)
                  then pure (Just (cast input))
                  else pure Nothing

guess : (target : Nat) -> IO ()
guess target = do putStr "Guess the number :) "
                  Just n <- readNumber | Nothing => putStr "Not a number :("
                  case (compare n target) of
                       GT => do putStr "You guess is lower than the number"
                                guess target
                       LT => do putStr "You guess is lower than the number"
                                guess target
                       EQ => putStr "Right answer"
