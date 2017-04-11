--sums up numerical indices in a list
sumList :: Num t => [t] -> t
sumList [] = 0
sumList (x : xs) = x + sumList xs

--creates all possible sublists from a list in which the list elements are in sequence.
createSubLists :: [t] -> [[t]]
createSubLists [] = []
createSubLists (x : xs) = createSubListSequence (x : xs) ++ createSubLists xs
	where 
		--creates a sublist with the possible sequences containing the first element
		createSubListSequence :: [t] -> [[t]]
		createSubListSequence [] = []
		createSubListSequence xs = [xs] ++ xy
			where
				xy = createSubListSequence $ take ((length xs) - 1) xs

--sorts a list of numerical lists based on the sum of each list. List comes in
sortOnListSum :: (Num t, Ord t) => [[t]] -> [[t]]
sortOnListSum [] = []
sortOnListSum (x : xs) =  sortOnListSum lowerSumLists ++ [x] ++ sortOnListSum higherSumLists
	where
		lowerSumLists = [y | y <- xs, sumList y <= sumList x]
		higherSumLists = [y | y <- xs, sumList y > sumList x]

--builds a string from a list in which every element is converted to a string separated by a blank row.
buildOutputString :: Show t => [t] -> String
buildOutputString [] = ['\n']
buildOutputString (x : xs) = ['\n'] ++ show x  ++ buildOutputString xs

--main function to find smallest k set
smallestK :: (Show t, Num t, Ord t) => [t] -> Int -> String
smallestK xs k = buildOutputString . take k . sortOnListSum . createSubLists $ xs

main = do
    putStrLn (smallestK [-1, 2, -3, 4, -5] 3)