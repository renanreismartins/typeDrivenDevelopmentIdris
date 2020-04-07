printLength : IO ()
printLength = getLine >>= \input => let len = length input in
                                        putStrLn (show len)


max : Nat -> Nat -> Nat
max n1 n2 = if n1 > n2 then n1 else n2

printLonger : IO ()
printLonger = do putStr "First string: "
                 first <- getLine
                 putStr "Second string: "
                 second <- getLine
                 putStr (show (Main.max (length first) (length second)))
