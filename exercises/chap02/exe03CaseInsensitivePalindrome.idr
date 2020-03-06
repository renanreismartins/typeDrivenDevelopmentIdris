iPalindrome : String -> Bool
iPalindrome str = let loweredStr = toLower str
                      reversedStr = reverse loweredStr in
                      reversedStr == loweredStr
