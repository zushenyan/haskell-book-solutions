module SquareCube where

mySqr = [ x ^ 2 | x <- [1 .. 5] ]
myCube = [ y ^ 3 | y <- [1 .. 5] ]

exe1 = [ (x, y) | x <- mySqr, y <- myCube ]
exe2 = [ (x, y) | x <- mySqr, x < 50, y <- myCube, y < 50 ]
exe3 = length exe2
