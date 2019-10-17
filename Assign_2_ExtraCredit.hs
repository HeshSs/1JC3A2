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

maxGaussNorm :: (Integral a, GaussianIntegral g, Eq (g a)) => [g a] -> g a
maxGaussNorm gs 
           | gs == [] = gaussZero
           | (length gs) == 1 = head gs
           | otherwise = if gaussNorm x0 < gaussNorm x1
                            then maxGaussNorm ([x1] ++ xs)
                            else maxGaussNorm ([x0] ++ xs)
                                    where x0 = head gs
                                          x1 = head (tail gs)
                                          xs = tail (tail gs)
