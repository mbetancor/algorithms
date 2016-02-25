module Cruise where
import Data.List 


-- Returns the max dotproduct between a list and a list of lists
filter_vector :: [Int] -> [[Int]] -> Int
filter_vector v1 vectorlist  =  maximum [ dotproduct x v1 | x <- vectorlist]

-- Returns the max dotprod between lists, using filter_vector
filter_dotprod :: [[Int]] -> Int
filter_dotprod [x] = 0
filter_dotprod (x:xs) = maximum $ filter_vector x xs:[filter_dotprod xs]

-- This is some kind of dotproduct
dotproduct :: [Int] -> [Int] -> Int
dotproduct list1 list2 = length $ list1 `intersect` list2

-- Returns the same function without duplicates
unique :: (Eq a) => [a] -> [a] 
unique []       = []
unique (x : xs) = x : unique (filter (x /=) xs)

-- Simple Permutation
perm :: [Int] -> [[Int]]
perm []  = [[]]
perm xs = unique [(x:ys) | x <- xs, ys <- perm (delete x xs)]

slice :: [Int] -> Int -> [[Int]]
slice [] _ = []
slice list index = (take index list):(slice (drop index list) index)

del :: [Int] ->[[Int]]
del list = [ delete x list |  x <- list] -- map delete list list

map_del :: [[Int]] -> Int -> [[Int]]
map_del list 0 = list
map_del list counter = map_del (concat (map del list)) (counter-1) 

crazy_mix :: Int -> Int -> [[Int]]
crazy_mix diners chairs = let 
  list = slice [1..diners] chairs
  exceed = ((diners `div` chairs) - chairs )
      in sort $ unique $ map_del (search [[]] list) exceed

search :: [[Int]] -> [[Int]] -> [[Int]]
search list []  = list
search list (x:xs)  =
  let new_list = [ l++[rest] | l <- list, rest <- x]
       in (search new_list xs) 

nonsat :: [Int] -> [[Int]]
nonsat [] = []
nonsat (x:xs) =
  let comb = [ [x,rest]| rest <- xs]
    in (comb ++ (nonsat xs) )

--delete_not_containing :: [Int] -> [Int] -> [Int]
--delete_not_containing ref_list list = 
--  if ( (length(ref_list `intersect` list)) == (length(list)) ) then list else []

delete_many :: Eq a => [a] -> [a] -> [a]
delete_many [] list = list
delete_many (x:xs) list = 
  delete_many xs (delete x list)

--deep :: Int -> Int -> Int -> Bool
--deep diners chairs evenings = 
--	let tables = diners `div` chairs
--		list = (slice [1..diners] chairs)
--		nonsat_list = concat $ map nonsat list
--		in cnf diners chairs evenings nonsat_list [1]



not_containing :: [Int] -> [[Int]] -> Bool
not_containing _ [[]] = True
not_containing _ [] = True
not_containing list (x:xs) = if ( (dotproduct list x) == length(x)) then False else (not_containing list xs)


find_available_table :: Int -> Int -> [[Int]] -> [Int] -> [[Int]] ->[Int]
find_available_table _ 0 _ temp_diners _ = temp_diners
find_available_table diners chairs nonsat_biglist temp_diners temp_nonsat = 
	let (_:rest) = temp_diners
	    aux_list = delete_many temp_diners [1..diners] 
	    a = [  x | x <- aux_list, (filter_vector (x:temp_diners) nonsat_biglist) < 2,
	       (not_containing (x:temp_diners) temp_nonsat == True) ]
	  in if (a /= []) then (find_available_table diners (chairs-1) nonsat_biglist ((head(a)):temp_diners) temp_nonsat)
	  	else (if (temp_diners == []) then [] 
	  	else (find_available_table diners (chairs+1) nonsat_biglist rest (temp_diners:temp_nonsat) ) )


--set_tables :: [Int] -> Int -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
--set_tables (first_diner:rest_of_diners) chairs  

--cnf :: Int -> Int -> Int -> [[Int]] -> [Int] -> Bool
--cnf diners chairs 1 _ list = (length(list)==(diners `div` chairs)) 
--cnf diners chairs evenings nonsat_list list = 






