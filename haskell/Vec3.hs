-- You do not need to modify this module.
module Vec3 (Vec3(..), scale, norm, normalise, cross, dot) where

import Control.DeepSeq

data Vec3 = Vec3 {vecX :: Float, vecY :: Float, vecZ :: Float}
  deriving (Eq, Ord, Show, Read)

instance NFData Vec3 where
  rnf (Vec3 x y z) = x `deepseq` y `deepseq` z `deepseq` ()

instance Num Vec3 where
  Vec3 x1 y1 z1 + Vec3 x2 y2 z2 = Vec3 (x1+x2) (y1+y2) (z1+z2)
  Vec3 x1 y1 z1 - Vec3 x2 y2 z2 = Vec3 (x1-x2) (y1-y2) (z1-z2)
  Vec3 x1 y1 z1 * Vec3 x2 y2 z2 = Vec3 (x1*x2) (y1*y2) (z1*z2)
  negate (Vec3 x y z) = Vec3 (negate x) (negate y) (negate z)
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  fromInteger a = Vec3 (fromInteger a) (fromInteger a) (fromInteger a)

instance Fractional Vec3 where
  recip (Vec3 x y z) = Vec3 (recip x) (recip y) (recip z)
  fromRational a = Vec3 (fromRational a) (fromRational a) (fromRational a)

scale :: Float -> Vec3 -> Vec3
scale a (Vec3 x y z) = Vec3 (a*x) (a*y) (a*z)

dot :: Vec3 -> Vec3 -> Float
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  x1*x2 + y1*y2 + z1*z2

norm :: Vec3 -> Float
norm v = sqrt (dot v v)

normalise :: Vec3 -> Vec3
normalise v = (1 / norm v) `scale` v

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3 (y1*z2-z1*y2) (z1*x2-x1*z2) (x1*y2-y1*x2)
