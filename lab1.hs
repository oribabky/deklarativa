
--sums up numerical indices in a list
sumList :: Num t => [t] -> t
sumList [] = 0
sumList (x : xs) = x + sumList xs

--creates all possible sub-sequences of a list in the format of quadruples containing the size, start/end index and the sublist itself.
createSubListSequences :: Num t => [t] -> [(t, Int, Int, [t])]
createSubListSequences [] = []
createSubListSequences xs = [subListQuad xs i j | i <- [1 .. len], j <- [1 .. len], j >= i]
	where
		len = length xs

--creates a sublist quadruple from a given range (i:j) with format (SIZE, i, j, LIST)
subListQuad :: Num t => [t] -> Int -> Int -> (t, Int, Int, [t])
sublistQuad [] _ _ = ()
subListQuad xs i j = (size, i, j, list)
	where
		list = drop (i - 1) . take j $ xs
		size = sumList list

--sorts a list of quads based on the first value in each quadruple. Variant of quicksort.
sortOnSize :: (Ord t, Num t) => [(t, Int, Int, [t])] -> [(t, Int, Int, [t])]
sortOnSize [] = []
sortOnSize (x : xs) = sortOnSize lowerSizeLists ++ [x] ++ sortOnSize higherSizeLists
	where
		lowerSizeLists = [y | y <- xs, getFirstElementQuad y <= getFirstElementQuad x]
		higherSizeLists = [y | y <- xs, getFirstElementQuad y > getFirstElementQuad x]

--Used to fetch the first element in a quadruple
getFirstElementQuad :: (a, b, c, d) -> a
getFirstElementQuad (a, b, c, d) = a

--returns any element in a quadruple as a string
getElementQuadString :: (Show a, Show b, Show c, Show d) => (a, b, c, d) -> Int -> String
getElementQuadString (a, b, c, d) element
	|element == 1 = show a
	|element == 2 = show b
	|element == 3 = show c
	|element == 4 = show d
	|otherwise = ""

--builds a string from a list in which every element is converted to a string separated by a blank row.
buildOutputString :: (Show t, Num t) => [(t, Int, Int, [t])] -> String
buildOutputString xs = bufferSpace "size" buffer ++ bufferSpace "i" buffer ++ bufferSpace "j" buffer ++ "list" ++ printRows xs buffer
	where
		buffer = 10

--Returns a string to represent one row in the printout which is one quadruple separated by blankspace
printRows :: (Show t, Num t) => [(t, Int, Int, [t])] -> Int -> String
printRows [] _ = ""
printRows (x : xs) buffer = ['\n'] ++ bufferSpace element1 buffer ++ bufferSpace element2 buffer ++ bufferSpace element3 buffer ++ element4 ++ printRows xs buffer
	where
		element1 = getElementQuadString x 1
		element2 = getElementQuadString x 2
		element3 = getElementQuadString x 3
		element4 = getElementQuadString x 4

--creates a buffer space between strings
bufferSpace :: String -> Int -> String
bufferSpace s k = s ++ take (k - len) [' ', ' ' ..]
	where
		len = length s

--main function to find smallest k set
smallestK :: (Show t, Num t, Ord t) => [t] -> Int -> String
smallestK [] k = error "Need an input list with at least one element!"
smallestK xs k = buildOutputString . take k . sortOnSize . createSubListSequences $ xs

--main procedure, call this to invoke smallestK
main' :: (Show t, Num t, Ord t) => [t] -> Int -> IO ()
main' xs i = do putStrLn (smallestK xs i)

main = do
	main' [x*(-1)^x | x <- [1..100]] 15
	main' [24,-11,-34,42,-24,7,-19,21] 6
	main' [] 6