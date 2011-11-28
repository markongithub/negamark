module Main where
  import Negamark
  import ProductGame

  main = putStrLn (show (fst (negamark (foldr newProductGameStateFromMove newProductGame (reverse [(4,7), (1,7), (3,7), (1,3), (3,6), (1,6), (5,6), (1,5), (1,4)])) 8 (Outcome Win 37 0) (Outcome Win 36 0))))
