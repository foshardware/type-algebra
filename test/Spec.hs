
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Spec.Type.Algebra.Goedel


main :: IO ()
main = defaultMain typeAlgebras


typeAlgebras :: TestTree
typeAlgebras = testGroup "Type algebra"
  [ goedels
  ]

