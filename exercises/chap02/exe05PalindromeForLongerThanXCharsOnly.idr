isLongPalindrome : Nat -> String ->  Bool
isLongPalindrome n str = let loweredStr = toLower str
                             reversedStr = reverse loweredStr in
                             (reversedStr == loweredStr) && (length loweredStr > n)
