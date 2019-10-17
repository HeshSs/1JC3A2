{- Assignment 2
 - Name: Hishmat Salehi
 - Date: 16/10/2019
 -}
module Assign_2 where

macid :: String
macid = "Salehh6"

type GaussianInt = (Integer,Integer)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description: Returns the Real part of the Gaussian Integer
 -}
gaussReal :: GaussianInt -> Integer
gaussReal (x,y) = x

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description: Returns the Imaginary part of the Gaussian Integer
 -}
gaussImag :: GaussianInt -> Integer
gaussImag (x,y) = y


{- -----------------------------------------------------------------
 - gaussConj
 - -----------------------------------------------------------------
 - Description: Returns the conjugate of the inputted Gaussian Integer
 -}
gaussConj :: GaussianInt -> GaussianInt
gaussConj g = (gaussReal g, - (gaussImag g))

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description: Adds 2 Gaussian Integers and returns the result
 -}
gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd g0 g1 = ((gaussReal g0 + gaussReal g1), (gaussImag g0 + gaussImag g1))

{- -----------------------------------------------------------------
 - gaussMult
 - -----------------------------------------------------------------
 - Description: Multiplies 2 Gaussian Integers and returns the result
 -}
gaussMult :: GaussianInt -> GaussianInt -> GaussianInt
gaussMult g0 g1 = ((gaussReal g0 * gaussReal g1) - (gaussImag g0 * gaussImag g1),(gaussReal g0 * gaussImag g1) + (gaussImag g0 * gaussReal g1))

{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description: The norm of a Gaussian integer is the multiplication of it with its conjugate
 -}
gaussNorm :: GaussianInt -> Integer
gaussNorm g = gaussReal (gaussMult g (gaussConj g))

{- -----------------------------------------------------------------
 - maxGaussNorm
 - -----------------------------------------------------------------
 - Description: Uses recursion to go through a list of Gaussian Integers and returns the one with the highest norm
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
 - - Function: gaussReal
 - - Test Case Number: 1
 - - Input: gaussReal (15,10)
 - - Expected Output: 15
 - - Actual Output: 15
 - -----------------------------------------------------------------
 - - Function: gaussReal
 - - Test Case Number: 2
 - - Input: gaussReal (1,5)
 - - Expected Output: 1
 - - Actual Output: 1
 - -----------------------------------------------------------------
 - - Function: gaussReal
 - - Test Case Number: 3
 - - Input: gaussReal (5,1)
 - - Expected Output: 5
 - - Actual Output: 5
 - -----------------------------------------------------------------
 - - Function: gaussImag
 - - Test Case Number: 1
 - - Input: gaussImag (15,10)
 - - Expected Output: 10
 - - Actual Output: 10
 - -----------------------------------------------------------------
 - - Function: gaussImag
 - - Test Case Number: 2
 - - Input: gaussImag (1,5)
 - - Expected Output: 5
 - - Actual Output: 5
 - -----------------------------------------------------------------
 - - Function: gaussImag
 - - Test Case Number: 3
 - - Input: gaussImag (5,1)
 - - Expected Output: 1
 - - Actual Output: 1
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 1
 - - Input: gaussConj (1,0)
 - - Expected Output: (1,0)
 - - Actual Output: (1,0)
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 2
 - - Input: gaussConj (5,10)
 - - Expected Output: (5,-10)
 - - Actual Output: (5,-10)
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 3
 - - Input: gaussConj (1,5)
 - - Expected Output: (1,-5)
 - - Actual Output: (1,-5)
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 1
 - - Input: gaussAdd (2,0) (1,2)
 - - Expected Output: (3,2)
 - - Actual Output: (3,2)
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 2
 - - Input: gaussAdd (-1,2) (1,2)
 - - Expected Output: (0,4)
 - - Actual Output: (0,4)
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 3
 - - Input: gaussAdd (-1,2) (3,-4)
 - - Expected Output: (2,-2)
 - - Actual Output: (2,-2)
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 1
 - - Input: gaussMult (2,0) (1,2)
 - - Expected Output: (2,4)
 - - Actual Output: (2,4)
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 2
 - - Input: gaussMult (2,3) (1,4)
 - - Expected Output: (-10,11)
 - - Actual Output: (-10,11)
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 3
 - - Input: gaussMult (1,2) (3,4)
 - - Expected Output: (-5,10)
 - - Actual Output: (-5,10)
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 1
 - - Input: gaussNorm (1,2)
 - - Expected Output: 5
 - - Actual Output: 5
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 2
 - - Input: gaussNorm (5,10)
 - - Expected Output: 125
 - - Actual Output: 125
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 3
 - - Input: gaussNorm (5,1)
 - - Expected Output: 26
 - - Actual Output: 26
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 1
 - - Input: maxGaussNorm [(3,6),(1,3),(3,5),(7,1),(6,4)]
 - - Expected Output: (6,4)
 - - Actual Output: (6,4)
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 2
 - - Input: maxGaussNorm [(1,2),(1,3),(6,5),(3,4),(3,3)]
 - - Expected Output: (6,5)
 - - Actual Output: (6,5)
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 3
 - - Input: maxGaussNorm [(5,3),(1,3),(3,5),(3,4),(3,3)]
 - - Expected Output: (5,3)
 - - Actual Output:(5,3)
 - -----------------------------------------------------------------
 
 -}

