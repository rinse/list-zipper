module Lib
    ( someFunc
    ) where

import Control.Comonad

-- class Functor w => Comonad w where
--     extract :: w a -> a
--     duplicate :: w a -> w (w a)
--     extend :: (w a -> b) -> w a -> w b

-- extend extract      = id
-- extract . extend f  = f
-- extend f . extend g = extend (f . extend g)


data Z a = Z [a] a [a] deriving Show

left, right :: Z a -> Z a
left (Z (l:ls) c rs) = Z ls l (c:rs)
right (Z ls c (r:rs)) = Z (c:ls) r rs

iterate1 :: (a -> a) -> a -> [a]
iterate1 f = tail . iterate f

instance Functor Z where
  fmap f (Z ls c rs) = Z (fmap f ls) (f c) (fmap f rs)

instance Comonad Z where
  extract (Z _ a _) = a
  duplicate z = Z (iterate1 left z) z (iterate1 right z)
  extend f z = Z (fmap f $ iterate1 left z) (f z) (fmap f $ iterate1 right z)

someFunc :: IO ()
someFunc = do
  let z = Z [3, 2, 1] 4 [5, 6, 7]
  print z
  print $ right $ left z
  print $ left z
  print $ right z

