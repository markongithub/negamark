module Main where
  import Negamark
  import ProductGame

  main = putStrLn $ show $ proveIsLoss modifiedProductGame 20
