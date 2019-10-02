{- Assignment 2
 - Name: Hishmat Salehi
 - Date: 5/10/2019
 -}
module Assign_2 where

macid :: String
macid = "Salehh6"

type GaussianInt = (Integer,Integer)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description: TODO add comments on gaussReal here
 -}
gaussReal :: GaussianInt -> Integer
gaussReal (x,y) = x

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description: TODO add comments on gaussImag here
 -}
gaussImag :: GaussianInt -> Integer
gaussImag (x,y) = y


{- -----------------------------------------------------------------
 - gaussConj
 - -----------------------------------------------------------------
 - Description: TODO add comments on gaussConj here
 -}
gaussConj :: GaussianInt -> GaussianInt
gaussConj g = (gaussReal g, - (gaussImag g))

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description: TODO add comments on gaussAdd here
 -}
gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd g0 g1 = ((gaussReal g0 + gaussReal g1), (gaussImag g0 + gaussImag g1))

{- -----------------------------------------------------------------
 - gaussMult
 - -----------------------------------------------------------------
 - Description: TODO add comments on gaussMult here
 -}
gaussMult :: GaussianInt -> GaussianInt -> GaussianInt
gaussMult g0 g1 = ((gaussReal g0 * gaussReal g1) - (gaussImag g0 * gaussImag g1),(gaussReal g0 * gaussImag g1) + (gaussImag g0 * gaussReal g1))

{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description: TODO add comments on gaussNorm here
 -}
gaussNorm :: GaussianInt -> Integer
gaussNorm g = gaussReal (gaussMult g (gaussConj g))

{- -----------------------------------------------------------------
 - maxGaussNorm
 - -----------------------------------------------------------------
 - Description: TODO add comments on maxGaussNorm here
 -}
maxGaussNorm :: [GaussianInt] -> GaussianInt
maxGaussNorm gs
           | gs == [] = (0,0)
           | (length gs) == 1 = head gs
           | otherwise = if gaussNorm x0 < gaussNorm x1
                            then maxGaussNorm ([x1] ++ xs)
                            else maxGaussNorm ([x0] ++ xs)
                                    where x0 = head gs
                                          x1 = head (tail gs)
                                          xs = tail (tail gs)


{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function:
 - - Test Case Number:
 - - Input:
 - - Expected Output:
 - - Acutal Output:
 - -----------------------------------------------------------------
 - TODO: add test cases
 -}

