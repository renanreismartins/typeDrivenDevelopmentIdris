module Main

import exe02Palindrome

printIsPalindrome : String -> String
printIsPalindrome str = show (palindrome str) ++ "\n"

main : IO ()
main = repl "Enter a string: " printIsPalindrome
