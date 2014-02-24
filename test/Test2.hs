
import Prelude                                          as P
import Data.Array.Accelerate                            as A
import qualified Data.Array.Accelerate.Interpreter      as I

import Data.Array.Accelerate.LLVM.Native
import Data.Array.Accelerate.LLVM.Debug

import System.Environment



b1 :: Acc (Vector Bool)
b1 = use (fromList (Z:.4) [True,False,True,False])

b2 :: Acc (Vector Bool)
b2 = use (fromList (Z:.4) [True,False,False,True])


f1 :: Acc (Vector Float)
f1 = use (fromList (Z:.11) [ x/5 | x <- [-5..5] ])


i1 :: Acc (Vector Int)
i1 = use (fromList (Z:.11) [-5..5])

i2 :: Acc (Vector Int)
i2 = use (fromList (Z:.10) [1..])

i21 :: Acc (Array DIM2 Int)
i21 = use (fromList (Z:.5:.10) [ y*10+x | y <- [-2..2], x <- [0..9] ])


w8 :: Acc (Vector Word8)
w8 = use (fromList (Z:.10) [3,4,1,2,4,8,98,3,4,1])


str1 :: Acc (Vector Char)
str1 = use (fromList (Z:.18) "Tom Marvolo Riddle")

str2 :: Acc (Vector Char)
str2 = use (fromList (Z:.18) "I am Lord Voldemort")



main = print (run test)

test :: Acc (Vector Double)
test = let steps = use (A.fromList (Z:.10) (repeat 100000))
       in  A.map foo steps

foo :: Exp Int -> Exp Double
foo x
  = A.snd
  $ A.while (\iv -> A.fst iv <* x)
            (\iv -> let i  = A.fst iv
                        v  = A.snd iv
                        v' = A.while (\jv -> A.fst jv >* 0)
                                     (\jv -> let j   = A.fst jv
                                                 v'' = v + (A.fromIntegral i / 32235) / (A.fromIntegral j / 233422)
                                             in
                                             lift (j-1, v'') :: Exp (Int,Double))
                                     (lift (constant 32265,v) :: Exp (Int,Double) )
                    in lift (i+1, A.snd v'))
            (constant (0,0) :: Exp (Int,Double))

