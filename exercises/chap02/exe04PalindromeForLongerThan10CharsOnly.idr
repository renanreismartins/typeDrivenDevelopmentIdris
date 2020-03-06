isLongPalindrome : String -> Bool
isLongPalindrome str = let loweredStr = toLower str
                           reversedStr = reverse loweredStr in
                           (reversedStr == loweredStr) && (length loweredStr > 10)
