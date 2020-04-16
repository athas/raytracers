{-# LANGUAGE StrictData #-}
module BVH
  ( AABB(..)
  , BVH(..)
  , mkBVH
  )
where

import Control.DeepSeq
import Control.Monad.Par
import Data.Function
import Data.List
import Vec3

data AABB = AABB { aabbMin :: Vec3
                 , aabbMax :: Vec3
                 }

instance NFData AABB where
  rnf (AABB minv maxv) = minv `deepseq` maxv `deepseq` ()

surroundingBox :: AABB -> AABB -> AABB
surroundingBox box0 box1 =
  let small = Vec3
              (vecX (aabbMin box0) `min` vecX (aabbMin box1))
              (vecY (aabbMin box0) `min` vecY (aabbMin box1))
              (vecZ (aabbMin box0) `min` vecZ (aabbMin box1))
      big = Vec3
            (vecX (aabbMax box0) `max` vecX (aabbMax box1))
            (vecY (aabbMax box0) `max` vecY (aabbMax box1))
            (vecZ (aabbMax box0) `max` vecZ (aabbMax box1))
  in AABB small big

aabbCentre :: AABB -> Vec3
aabbCentre aabb =
  Vec3
  (vecX (aabbMin aabb) + (vecX (aabbMax aabb) - vecX (aabbMin aabb)))
  (vecY (aabbMin aabb) + (vecY (aabbMax aabb) - vecY (aabbMin aabb)))
  (vecZ (aabbMin aabb) + (vecZ (aabbMax aabb) - vecZ (aabbMin aabb)))

data BVH a = BVHLeaf AABB a
           | BVHSplit AABB (BVH a) (BVH a)

instance NFData (BVH a) where
  rnf (BVHLeaf box _) = box `deepseq` ()
  rnf (BVHSplit box x y) = box `deepseq` x `deepseq` y `deepseq` ()

bvhAABB :: BVH a -> AABB
bvhAABB (BVHLeaf box _) = box
bvhAABB (BVHSplit box _ _) = box

mkBVH :: (a -> AABB) -> [a] -> BVH a
mkBVH f all_objs = runPar $ mkBVH' (0::Int) (length all_objs) all_objs
  where mkBVH' _ _ [] = error "mkBVH: empty no nodes"
        mkBVH' _ _ [x] = return $ BVHLeaf (f x) x
        mkBVH' d n xs = do
          let n2 = n `div` 2
              d1 = d + 1
              (xs_left, xs_right) =
                splitAt n2 $ sortBy (compare `on` comparison) xs
              left = mkBVH' d1 n2 xs_left
              right = mkBVH' d1 (n - n2) xs_right
          (left', right') <-
            if n < 100
            then (,) <$> left <*> right
            else do left' <- spawn left
                    right' <- spawn right
                    (,) <$> get left' <*> get right'
          let box = bvhAABB left' `surroundingBox` bvhAABB right'
          return $ BVHSplit box left' right'
          where axis = case d `mod` 3 of
                         0 -> vecX
                         1 -> vecY
                         _ -> vecZ
                comparison = axis . aabbCentre . f
