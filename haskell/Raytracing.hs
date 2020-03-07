module Raytracing
  ( Pos, Dir, Camera, mkCamera
  , Sphere(..), sphereAABB
  , Objs
  , Colour, black, white
  , render
  )
where

import Data.Maybe (fromMaybe)
import BVH
import Image
import Vec3

type Pos = Vec3
type Dir = Vec3

type Colour = Vec3
black, white :: Colour
black = 0
white = 1

data Ray = Ray { rayOrigin :: Pos
               , rayDir :: Dir }
           deriving (Show)

pointAtParam :: Ray -> Float -> Pos
pointAtParam ray t = rayOrigin ray + (t `scale` rayDir ray)

data Hit = Hit { hitT :: Float
               , hitP :: Pos
               , hitNormal :: Dir
               , hitColour :: Colour
               }

data Sphere = Sphere { spherePos :: Pos
                     , sphereColour :: Colour
                     , sphereRadius :: Float }
            deriving (Show, Read)

sphereAABB :: Sphere -> AABB
sphereAABB (Sphere centre _ radius) =
  AABB
  (centre - Vec3 radius radius radius)
  (centre + Vec3 radius radius radius)

sphereHit :: Sphere -> Ray -> Float -> Float -> Maybe Hit
sphereHit (Sphere center colour radius) r t_min t_max =
  let oc = rayOrigin r - center
      a = dot (rayDir r) (rayDir r)
      b = dot oc (rayDir r)
      c = dot oc oc - radius*radius
      discriminant = b*b - a*c
      tryHit temp =
        if temp < t_max && temp > t_min
        then Just $ Hit
             { hitT = temp
             , hitP = pointAtParam r temp
             , hitNormal = (1/radius) `scale` (pointAtParam r temp - center)
             , hitColour = colour
             }
        else Nothing
  in if discriminant <= 0
     then Nothing
     else case tryHit ((-b - sqrt(b*b-a*c))/a) of
            Just hit -> Just hit
            Nothing -> tryHit ((-b + sqrt(b*b-a*c))/a)

type Objs = BVH Sphere

aabbHit :: AABB -> Ray -> Float -> Float -> Bool
aabbHit aabb (Ray origin direction) tmin0 tmax0 =
  let iter min' max' origin' dir' tmin' tmax' =
        let invD = 1 / dir'
            t0 = (min' - origin') * invD
            t1 = (max' - origin') * invD
            (t0', t1') = if invD < 0 then (t1, t0) else (t0, t1)
            tmin'' = max t0' tmin'
            tmax'' = min t1' tmax'
        in (tmin'', tmax'')
      (tmin1, tmax1) =
        iter
        (vecX (aabbMin aabb)) (vecX (aabbMax aabb))
        (vecX origin) (vecX direction)
        tmin0 tmax0
  in if tmax1 <= tmin1 then False
     else let (tmin2, tmax2) =
                iter (vecY (aabbMin aabb)) (vecY (aabbMax aabb))
                (vecY origin) (vecY direction)
                tmin1 tmax1
          in if tmax2 <= tmin2 then False
             else let (tmin3, tmax3) =
                        iter (vecZ (aabbMin aabb)) (vecZ (aabbMax aabb))
                        (vecZ origin) (vecZ direction)
                        tmin2 tmax2
                  in not (tmax3 <= tmin3)

objsHit :: Objs -> Ray -> Float -> Float -> Maybe Hit
objsHit (BVHLeaf _ sphere) r t_min t_max =
  sphereHit sphere r t_min t_max
objsHit (BVHSplit box left right) r t_min t_max
  | not $ aabbHit box r t_min t_max =
      Nothing
  | otherwise =
      case objsHit left r t_min t_max of
        Nothing -> objsHit right r t_min t_max
        Just h1 ->
          Just $ fromMaybe h1 $ objsHit right r t_min (hitT h1)

data Camera = Camera { camOrigin :: Pos
                     , camLLC :: Pos
                     , camHorizontal :: Dir
                     , camVertical :: Dir
                     }
              deriving (Show, Read)

mkCamera :: Pos -> Pos -> Dir -> Float -> Float -> Camera
mkCamera lookfrom lookat vup vfov aspect =
  let theta = vfov * pi / 180
      half_height = tan (theta / 2)
      half_width = aspect * half_height
      origin = lookfrom
      w = normalise (lookfrom - lookat)
      u = normalise (cross vup w)
      v = cross w u
  in Camera { camOrigin = lookfrom
            , camLLC = origin -
                       (half_width `scale` u) -
                       (half_height `scale` v) -
                       w
            , camHorizontal = (2*half_width) `scale` u
            , camVertical = (2*half_height) `scale` v
            }

getRay :: Camera -> Float -> Float -> Ray
getRay cam s t =
  Ray
  (camOrigin cam)
  (camLLC cam +
   (s `scale` camHorizontal cam) +
   (t `scale` camVertical cam) -
   camOrigin cam)

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - (2 * dot v n) `scale` n

scatter :: Ray -> Hit -> Maybe (Ray, Vec3)
scatter r hit =
  let reflected =
        reflect (normalise (rayDir r)) (hitNormal hit)
      scattered = Ray (hitP hit) reflected
  in if dot (rayDir scattered) (hitNormal hit) > 0
     then Just (scattered, hitColour hit)
     else Nothing

rayColour :: Objs -> Ray -> Int -> Colour
rayColour objs r depth =
  case objsHit objs r 0.001 (1/0) of
    Just hit ->
      case scatter r hit of
        Just (scattered, attenuation)
          | depth < 50 ->
              attenuation * (rayColour objs scattered (depth+1))
        _ -> black
    Nothing ->
      let unit_dir = normalise (rayDir r)
          t = 0.5 * (vecY unit_dir + 1)
      in ((1-t) `scale` Vec3 1 1 1) + (t `scale` Vec3 0.5 0.7 1)

traceRay :: Objs -> Int -> Int -> Camera -> Int -> Int -> Colour
traceRay objs width height cam =
  \j i -> let u = fromIntegral i / fromIntegral width
              v = fromIntegral j / fromIntegral height
              ray = getRay cam u v
          in rayColour objs ray 0

colourToPixel :: Colour -> Pixel
colourToPixel (Vec3 r g b) =
  let ir = truncate (255.99 * r)
      ig = truncate (255.99 * g)
      ib = truncate (255.99 * b)
  in (ir, ig, ib)

render :: Objs -> Int -> Int -> Camera -> Image
render objs width height cam =
  mkImage width height $ \j i ->
  colourToPixel $ traceRay objs width height cam j i
