numberOfWordsAndChars : String -> (Nat, Nat)
numberOfWordsAndChars str = let numberOfWords = length (words str) in
                                (numberOfWords,  length str) 
