-- Logical operators do not short circuit
-- https://github.com/AccelerateHS/accelerate/issues/292
--
import Data.Array.Accelerate              as A
import Data.Array.Accelerate.Debug        as A
import Data.Array.Accelerate.Interpreter  as I
import Data.Array.Accelerate.LLVM.Native  as CPU

test :: Acc (Vector Bool)
test =
  let xs :: Acc (Vector Int)
      xs = use (fromList (Z :. 10) [0..])
  in
  A.generate (constant (Z :. 20))
             (\ix -> unindex1 ix A.<* A.length xs &&* xs ! ix A.<* 10
                      ? ( constant True , constant False ))

