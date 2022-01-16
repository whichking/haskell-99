-- I've got ninety-nine problems, and they can all be solved using Haskell.

-- 1. Find the last element of a list
-- Obviously, we could just let lastElement = last, but that's no fun, is it?
lastElement :: [a] -> a
lastElement xs = case xs of [] -> error "no last element of a empty list"
                            [x] -> x
                            (_:xs) -> lastElement xs

-- 2. Find the penultimate element of a list
-- Sure, we could get the last element of init, but, again, NO FUN
penultimateElement :: [a] -> a
penultimateElement xs = case xs of [] -> error "no penultimate element of an empty list"
                                   [x] -> error "no penultimate element of a singleton list"
                                   [x, y] -> x
                                   (_:xs) -> penultimateElement xs

-- 3. Find the kth element of a list, where the first element is number 1
-- !! would make this too easy
indexSearch :: [a] -> Int -> a
indexSearch [] k = error "index is out of bounds"
indexSearch (x:_) 1 = x
indexSearch (x:xs) k = indexSearch xs (k - 1)

-- 4. Find the number of elements in a list
-- blah blah length blah blah boring
howLong :: [a] -> Int
howLong [] = 0
howLong [x] = 1
howLong (_:xs) = 1 + howLong xs

-- 5. Reverse a list
-- not even going to say it
flipIt :: [a] -> [a]
flipIt xs = case xs of [] -> []
                       [x] -> [x]
                       (x:xs) -> flipIt xs ++ [x]
