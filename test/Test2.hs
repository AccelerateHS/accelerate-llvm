
import Prelude                                          as P
import Data.Array.Accelerate                            as A
import qualified Data.Array.Accelerate.Interpreter      as I

import Data.Array.Accelerate.LLVM.Native
import Data.Array.Accelerate.LLVM.Debug



b1 :: Acc (Vector Bool)
b1 = use (fromList (Z:.4) [True,False,True,False])

b2 :: Acc (Vector Bool)
b2 = use (fromList (Z:.4) [True,False,False,True])


f1 :: Acc (Vector Float)
f1 = use (fromList (Z:.11) [ x/5 | x <- [-5..5] ])


i1 :: Acc (Vector Int)
i1 = use (fromList (Z:.11) [-5..5])


w8 :: Acc (Vector Word8)
w8 = use (fromList (Z:.10) [3,4,1,2,4,8,98,3,4,1])


str1 :: Acc (Vector Char)
str1 = use (fromList (Z:.18) "Tom Marvolo Riddle")

str2 :: Acc (Vector Char)
str2 = use (fromList (Z:.18) "I am Lord Voldemort")

