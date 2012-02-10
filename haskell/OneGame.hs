module Main where
  import Negamark
  import ProductGame

--  main = putStrLn (show (fst (negamark (foldr newProductGameStateFromMove newProductGame (reverse [(4,7), (1,7), (3,7), (1,3), (3,6), (1,6), (5,6), (1,5), (1,4)])) 8 (Outcome Loss 36 0) (Outcome Loss 37 0))))
  main = putStrLn (show (fst (negamark (foldr newProductGameStateFromMove newProductGame (reverse [(4,4)])) 18 (Outcome Loss 36 0) (Outcome Loss 37 0))))
--  main = putStrLn (show (head (snd (pickMove (foldr newProductGameStateFromMove newProductGame (reverse [])) 6))))
