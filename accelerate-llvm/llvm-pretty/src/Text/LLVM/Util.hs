module Text.LLVM.Util where

import Control.Monad (MonadPlus,mzero)
import Data.List (unfoldr)


breaks :: (a -> Bool) -> [a] -> [[a]]
breaks p = unfoldr step
  where
  step [] = Nothing
  step xs = case break p xs of
    (as,_:bs) -> Just (as,bs)
    (as,  []) -> Just (as,[])

uncons :: MonadPlus m => [a] -> m (a,[a])
uncons (a:as) = return (a,as)
uncons _      = mzero

