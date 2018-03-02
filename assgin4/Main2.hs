split :: [Int] -> ([Int],[Int])
split [] = ([], [])
split xs = ((x : fst (split xs)), (x : snd (split xs)))
