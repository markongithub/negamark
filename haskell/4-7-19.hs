module Main where
  import Negamark
  import ProductGame

  main = putStrLn (show (fst (negamark (foldr newProductGameStateFromMove newProductGame (reverse [(4,7)])) 18 (Outcome Loss 36 0) (Outcome Loss 37 0))))
