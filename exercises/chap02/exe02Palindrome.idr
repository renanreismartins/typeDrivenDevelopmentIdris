module Palindrome

export
palindrome : String -> Bool
palindrome str = reverse str == str
