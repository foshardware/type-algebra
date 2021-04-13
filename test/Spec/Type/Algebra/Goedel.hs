{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}

module Spec.Type.Algebra.Goedel where

import Data.Typeable

import Test.Tasty
import Test.Tasty.HUnit

import Type.Algebra.Goedel
import Type.Algebra.Natural



data Contrived = Contrive deriving Goedel

data ContrivedG a
  where
    ContriveA :: Int -> ContrivedG a
    ContriveB :: a -> ContrivedG a
  deriving Goedel



goedels :: TestTree
goedels = testGroup "Goedel"

  [ testCase "Derive numbers"
      $ do

        assertEqual "contrived" 480026597954659812
          $ goedel Contrive

        -- assertEqual "function" 6825683341122574805 (goedel $ id @P2)

        assertBool "IO"
          $ goedel (pure @IO True) /= goedel (pure @IO ())

  , testCase "Show"
      $ do

        assertEqual "()" "()"
          $ show $ typeOf ()

        assertEqual "ContrivedB ()" "ContrivedG ()"
          $ show $ typeOf (ContriveB ())

  ]

