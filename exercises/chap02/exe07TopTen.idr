topTen : Ord a => List a -> List a
topTen l = take 10 (reverse (sort l))
