module Main where
  import Negamark
  import ProductGame

  main = putStrLn $ show $ proveIsLoss (newProductGameStateFromMoveSet newProductGame [(4,7)]) 18
