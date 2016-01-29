module Progress where
import Data.List 
import System.Random

-- Some Notes:
-- dotproduct [1,2,3] [2,3,4] = len([2,3]) = 2.
-- This is the same as dotproduct [1,1,1,0] [0,1,1,1]. 

--dotprod :: [Int] -> [Int] -> Int
--dotprod a b  = sum $ (zipWith (*) a b)

-- Returns the max dotproduct between a list and a list of lists
filter_vector :: [Int] -> [[Int]] -> Int
filter_vector v1 vectorlist  =  maximum [ dotproduct x v1 | x <- vectorlist]

-- Returns the max dotprod between two lists, using filter_vector
filter_dotprod :: [[Int]] -> Int
filter_dotprod [x] = 0
filter_dotprod (x:xs) = maximum (filter_vector x xs:[filter_dotprod xs])


-- A particular permutation, because it only takes combinations of length=limit
perm :: [Int] -> Int -> [[Int]]
perm [] _ = [[]]
perm _ 0 = [[]]
perm xs limit = unique [sort(x:ys) | x <- xs, ys <- perm (delete x xs) (limit-1)]

-- Returns the same function without duplicates
unique :: [[Int]] -> [[Int]]
unique []       = []
unique (x : xs) = x : unique (filter (x /=) xs)


-- This is some kind of dotproduct
dotproduct :: [Int] -> [Int] -> Int
dotproduct list1 list2 = length(list1 `intersect` list2)

remove_lists :: [[Int]] -> [[Int]] -> [[Int]]
remove_lists list [] = list
remove_lists [] list = []
remove_lists lists (x:xs) = remove_lists (remove_list lists x ) xs

-- Removes a list from a list of lists
remove_list :: [[Int]] -> [Int] -> [[Int]]
remove_list [] _ = []
remove_list (x:xs) element 
			| x==element = remove_list xs element
			| otherwise = x:(remove_list xs element) 

-- Removes an element from a list
remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (x:xs) element 
			| x==element = remove xs element
			| otherwise = x:(remove xs element) 


-- INCOMPLETE! We need the random package 
first_solution :: Int -> Int -> Int -> [[Int]]
first_solution rows columns r =  shuffle_m (perm [1..columns] r) (length(perm [1..columns] r) `div` rows)

-- Alternative to random. Fix it if possible..
shuffle_m :: [[Int]] -> Int -> [[Int]]
shuffle_m list partitions = 
		let positions = (takeWhile (<length(list)) [n*partitions| n<-[0..]])
		in [ list !! n | n <- positions]
-- Pseudo Fix, tomorrow I will add the random stuff
shuffle_m2 :: [[Int]] -> [Int] -> [[Int]]
shuffle_m2 list partitions = 
		 [ list !! n | n <- partitions]

-- This can be extended to all the posibilities
get_first_answer :: Int -> Int -> Int -> Int
get_first_answer rows columns r = filter_dotprod (first_solution rows columns r)

guilty_ones :: [[Int]] -> Int -> [[Int]]
guilty_ones list limit = unique [ x | x <- list, y <- list, limit == dotproduct x y]

rollDice :: Int -> IO Int
rollDice limit = getStdRandom (randomR (0,limit))

-- Improve this ugly function. Seriously, it´s awfull, shame on you.
try_changes :: [[Int]] -> [[Int]] ->[[Int]] -> Int -> [[Int]]
try_changes [] curr_sol_list _ _ = curr_sol_list
try_changes changes curr_sol_list total_list curr_sol_lambda  =
	let
		(x:xs) = changes
		neighbors = filter (>x) (remove_lists total_list curr_sol_list)
		alternative = ( if neighbors /= [] then head(neighbors) else [-1])
		alt_ans = (if alternative /= [-1] then filter_vector alternative (remove_list curr_sol_list x) else -1)
		new_sol = (if (( alt_ans <0) || ( alt_ans >curr_sol_lambda)) then curr_sol_lambda else alt_ans)
		new_sol_list = (if new_sol==alt_ans then sort(alternative:(remove_list curr_sol_list x)) else curr_sol_list)
	in
		try_changes xs new_sol_list total_list new_sol 


--main :: Int -> Int -> Int -> Int
--main rows columns r  = begin complete_list curr_sol_list tabu_list curr_sol_lambda
--	where complete_list = perm [1..columns] r
--		curr_sol_list = take rows complete_list
--		tabu_list = [[]]
--		curr_sol_lambda = filter_dotprod (curr_sol_list)

--begin :: [[Int]] -> [[Int]] -> [[Int]] -> Int
--begin total_list curr_sol_list tabu_list curr_sol_lambda = 
--	let changes = unique (guilty_ones curr_sol_list curr_sol_lambda )
--	try_changes

--main ::
--main rows columns r =
--	big_list = perm[1..columns] 




