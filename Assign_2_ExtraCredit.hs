{-# LANGUAGE FlexibleContexts,FlexibleInstances,IncoherentInstances #-}
{- Assignment 2 Extra Credit
 - Name:Hishmat Salehi
 - Date: 16/10/2019
 -}
module Assign_2_ExtraCredit where

import Data.Complex

macid = "Salehh6"


data GaussianInt a = a :@ a
  deriving Show

class GaussianIntegral g where
  gaussZero :: Integral a => g a
  gaussReal :: Integral a => g a -> a
  gaussImag :: Integral a => g a -> a
  gaussConj :: Integral a => g a -> g a
  gaussAdd :: Integral a => g a -> g a -> g a
  gaussMult :: Integral a => g a -> g a -> g a

instance GaussianIntegral GaussianInt where
    gaussZero = (0 :@ 0)
    gaussReal (x :@ y) = x
    gaussImag (x :@ y) = y
    gaussConj (x :@ y) = (x :@ (-y))
    gaussAdd (a0 :@ b0) (a1 :@ b1) = ((a0 + a1) :@ (b0 + b1))
    gaussMult (a0 :@ b0) (a1 :@ b1) = (((a0 * a1) - (b0 * b1)) :@ ((a0 * b1) + (b0 * a1)))

instance GaussianIntegral Complex where
    gaussZero = (0 :+ 0)
    gaussReal (x :+ y) = x
    gaussImag (x :+ y) = y
    gaussConj (x :+ y) = (x :+ (-y))
    gaussAdd (a0 :+ b0) (a1 :+ b1) = ((a0 + a1) :+ (b0 + b1))
    gaussMult (a0 :+ b0) (a1 :+ b1) = (((a0 * a1) - (b0 * b1)) :+ ((a0 * b1) + (b0 * a1)))

instance (Integral a) => Eq (GaussianInt a) where
    (a0 :@ b0) == (a1 :@ b1) = (a0 == a1) && (b0 == b1)

instance (Integral a) => Ord (GaussianInt a) where
    (a0 :@ b0) < (a1 :@ b1) = (a0 < a1) && (b0 < b1)
    (a0 :@ b0) > (a1 :@ b1) = (a0 > a1) && (b0 > b1)
    (a0 :@ b0) >= (a1 :@ b1) = (a0 >= a1) && (b0 >= b1)
    (a0 :@ b0) <= (a1 :@ b1) = (a0 <= a1) && (b0 <= b1)
    max x y 
        | x >= y    =  x
        | otherwise =  y
    min x y
        | x <  y    =  x
        | otherwise =  y

gaussNorm :: (Integral a, GaussianIntegral g) => g a -> a
gaussNorm g = gaussReal (gaussMult g (gaussConj g))

maxGaussNorm :: (Integral a, GaussianIntegral g, Eq (g a), Ord (g a)) => [g a] -> g a
maxGaussNorm gs = foldr max gaussZero gs

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -

 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 1
 - - Input: gaussConj (1 :@ 0)
 - - Expected Output: (1 :@ 0)
 - - Actual Output: (1 :@ 0)
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 2
 - - Input: gaussConj (5 :@ 10)
 - - Expected Output: 5 :@ (-10)
 - - Actual Output: 5 :@ (-10)
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 3
 - - Input: gaussConj (1 :@ 5)
 - - Expected Output: 1 :@ (-5)
 - - Actual Output: 1 :@ (-5)
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 1
 - - Input: gaussAdd (2 : @0) (1 : @2)
 - - Expected Output: 3 :@ 2
 - - Actual Output: 3 :@ 2
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 2
 - - Input: gaussAdd (3 :@ 2) (1 :@ 2)
 - - Expected Output: 4 :@ 4
 - - Actual Output: 4 :@ 4
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 3
 - - Input: gaussAdd (1 :@ 2) (3 :@ 4)
 - - Expected Output: 4 :@ 6
 - - Actual Output: 4 :@ 6
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 1
 - - Input: gaussMult (2 :@ 0) (1 :@ 2)
 - - Expected Output: 2 :@ 4
 - - Actual Output: 2 :@ 4
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 2
 - - Input: gaussMult (2 :@ 3) (1 :@ 4)
 - - Expected Output: (-10) :@ 11
 - - Actual Output: (-10) :@ 11
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 3
 - - Input: gaussMult (1:@2) (3:@4)
 - - Expected Output: (-5) :@ 10
 - - Actual Output: (-5) :@ 10
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 1
 - - Input: gaussNorm (1 :@ 2)
 - - Expected Output: 5
 - - Actual Output: 5
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 2
 - - Input: gaussNorm (5:@10)
 - - Expected Output: 125
 - - Actual Output: 125
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 3
 - - Input: gaussNorm (5:@1)
 - - Expected Output: 26
 - - Actual Output: 26
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 1
 - - Input: maxGaussNorm [(3:@6),(1:@3),(3:@5),(7:@1),(6:@4)]
 - - Expected Output: 6 :@ 4
 - - Actual Output: 6 :@ 4
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 2
 - - Input: maxGaussNorm [(1:@2),(1:@3),(6:@5),(3:@4),(3:@3)]
 - - Expected Output: 6 :@ 5
 - - Actual Output: 6 :@ 5
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 3
 - - Input: maxGaussNorm [(5:@3),(1:@3),(3:@5),(3:@4),(3:@3)]
 - - Expected Output: 5 :@ 3
 - - Actual Output: 3 :@ 5
 - -----------------------------------------------------------------
 
 -}

