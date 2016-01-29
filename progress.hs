module Progress where
import Data.List 


-- Some Notes:
-- dotproduct [1,2,3] [2,3,4] = len([1,4]) = 2.
-- This is the same as dotproduct [1,1,1,0] [0,1,1,1]. 

--dotprod :: [Int] -> [Int] -> Int
--dotprod a b  = sum $ (zipWith (*) a b)

-- Returns the max dotproduct between a list and a list of lists
filter_vector :: [Int] -> [[Int]] -> Int
filter_vector v1 vectorlist  =  maximum [ dotproduct x v1 | x <- vectorlist]

-- Returns the max dotprod between two lists, using filter_vector
filter_dotprod :: [[Int]] -> Int
filter_dotprod [x1,x2] = dotproduct x1 x2
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

-- The name is confusing, this is just the union of two lists without the elements in common 
remove_dupls :: [Int] -> [Int] -> [Int]
remove_dupls [] b = b
remove_dupls (x:xs) b 
		| x `elem` b =  remove_dupls xs (remove b x)
		| otherwise = x:(remove_dupls xs b)

-- This is some kind of dotproduct
dotproduct :: [Int] -> [Int] -> Int
dotproduct list1 list2 = length(remove_dupls list1 list2)

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

-- Can we say this is a random solution? :D
first_solution :: Int -> Int -> Int -> [[Int]]
first_solution rows columns r = take rows (perm [1..columns] r)

-- This can be extended to all the posibilities
get_first_answer :: Int -> Int -> Int -> Int
get_first_answer rows columns r = filter_dotprod (first_solution rows columns r)

guilty_ones :: [[Int]] -> Int -> [[Int]]
guilty_ones list limit = [ x | x <- list, y <- list, limit == dotproduct x y]

--neighbors :: [Int] -> [[Int]] -> [[Int]]
--neighbors element lists = ( ( find (< element) lists): (find (> element) lists))


--main :: Int -> Int -> Int -> Int
--main  columns rows r  = begin complete_list curr_sol_list tabu_list curr_sol_lambda
--	where complete_list = perm [1..columns] r
--		curr_sol_list = take rows complete_list
--		tabu_list = [[]]
--		curr_sol_lambda = filter_dotprod (curr_sol_list)

--begin :: [[Int]] -> [[Int]] -> [[Int]] -> Int
--begin total_list curr_sol_list tabu_list curr_sol_lambda = 
--	let changes = remove_dupls (guilty_ones curr_sol_list curr_sol_lambda )
--	try_changes

-- Improve this ugly function. Seriously, it´s awfull, shame on you.


try_changes :: [[Int]] -> [[Int]] ->[[Int]] -> Int -> [[Int]]
try_changes [] curr_sol_list _ _ = curr_sol_list
try_changes changes curr_sol_list total_list curr_sol_lambda  =
	let
		(x:xs) = changes
		(neighbor:_) = filter (>x) (remove_lists total_list curr_sol_list)
	|

	let 
		(x:xs) = changes
		neighbors = filter (>x) (remove_lists total_list curr_sol_list)
		alternative = ( if neighbors /= [] then head(neighbors) else -1)
		alt_ans = (if alternative /= -1 then filter_vector alternative (remove_list curr_sol_list x) else -1)
		(new_sol_list, new_sol) =
			(if (alt_ans >0) and (alt_ans<=curr_sol_lambda)
				then (sort(alternative:(remove_list x curr_sol_list)),alt_ans)
			else  (curr_sol_list,curr_sol_lambda))
	in
		try_changes xs new_sol_list total_list new_sol 
-- REHACER!!!









--main ::
--main rows columns r =
--	big_list = perm[1..columns] 

---- IDEA: Vecinos de [1,2,3] = [2,3,4], [1,3,4],[1,2,4]


