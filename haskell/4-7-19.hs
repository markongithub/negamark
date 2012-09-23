module Main where
  import Negamark
  import ProductGame

  main = putStrLn (show (fst (negamark (newProductGameStateFromMoveSet newProductGame [(4,7)]) 18 (Loss 36) (Loss 37))))
