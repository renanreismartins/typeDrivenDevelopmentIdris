module Main
import System

readNumber : IO (Maybe Nat)
readNumber = do input <- getLine
                if all isDigit (unpack input)
                  then pure (Just (cast input))
                  else pure Nothing

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do putStr (show guesses ++ " attempts so far\n")
                          putStr "Guess the number :) "
                          Just n <- readNumber | Nothing => putStr "Not a number\n"
                          case (compare n target) of
                            GT => do putStr "You guess is lower than the number\n"
                                     guess target (S guesses)
                            LT => do putStr "You guess is lower than the number\n"
                                     guess target (S guesses)
                            EQ => putStr "Right answer\n"

main : IO ()
main = do
  t <- time
  let rand = cast (1 + (mod t 100)) -- adds 1 otherwise will be from 0 to 99
  guess rand 0


myReplWith : (state : a) -> (prompt : String) -> (onInput: a -> String -> Maybe (String, a)) -> IO ()
myReplWith state prompt onInput = do putStr prompt
                                     input <- getLine
                                     case (onInput state input) of
                                       Just (output, result) => do putStr output
                                                                   myReplWith result prompt onInput
                                       Nothing => pure ()

{- FOUND ON INTERNET - PATTERN MATCHING AND USING THE VARS BINDED FROM IT
 my_replWith state prompt onInput = do
   putStr prompt
   input <- getLine
   let Just(output, newState) = onInput state input
       | Nothing => pure ()
   putStrLn output
   my_replWith newState prompt onInput -}
