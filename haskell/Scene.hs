module Scene (Scene(..), fromScene,
              rgbbox, irreg) where

import BVH
import Raytracing
import Vec3

data Scene = Scene { sceneCamLookFrom :: Pos
                   , sceneCamLookAt :: Pos
                   , sceneCamFov :: Float
                   , sceneSpheres :: [Sphere]
                   }
           deriving (Show, Read)

fromScene :: Int -> Int -> Scene -> (Objs, Camera)
fromScene width height scene =
  (mkBVH sphereAABB $ sceneSpheres scene,
   mkCamera (sceneCamLookFrom scene) (sceneCamLookAt scene) (Vec3 0 1 0)
   (sceneCamFov scene) (fromIntegral width/fromIntegral height))

rgbbox :: Scene
rgbbox = Scene { sceneSpheres = leftwall ++ midwall ++ rightwall ++ bottom
               , sceneCamLookFrom = Vec3 0 30 30
               , sceneCamLookAt = Vec3 0 (-1) (-1)
               , sceneCamFov = 75 }
  where n = 10
        k = 60
        leftwall =
          [ Sphere (Vec3
                    (-k/2)
                    (-k/2 + (k/n) * y)
                    (-k/2 + (k/n) * z))
            (Vec3 1 0 0) (k/(n*2))
          | y <- [0..n-1], z <- [0..n-1]
          ]
        midwall =
          [ Sphere (Vec3
                     (-k/2 + (k/n) * x)
                     (-k/2 + (k/n) * y)
                     (-k/2))
            (Vec3 0 1 0) (k/(n*2))
          | x <- [0..n-1], y <- [0..n-1]
          ]
        rightwall =
          [ Sphere (Vec3
                     (k/2)
                     (-k/2 + (k/n) * y)
                     (-k/2 + (k/n) * z))
            (Vec3 0 0 1) (k/(n*2))
          | y <- [0..n-1], z <- [0..n-1]
          ]
        bottom =
          [ Sphere (Vec3
                     (-k/2 + (k/n) * x)
                     (-k/2)
                     (-k/2 + (k/n) * z))
            (Vec3 1 1 1) (k/(n*2))
          | x <- [0..n-1], z <- [0..n-1]
          ]

irreg :: Scene
irreg = Scene { sceneSpheres = bottom
              , sceneCamLookFrom = Vec3 0 12 30
              , sceneCamLookAt = Vec3 0 10 (-1)
              , sceneCamFov = 75 }
  where n = 100
        k = 600
        bottom =
          [ Sphere (Vec3
                     (-k/2 + (k/n) * x)
                     0
                     (-k/2 + (k/n) * z))
            (Vec3 1 1 1) (k/(n*2))
          | x <- [0..n-1], z <- [0..n-1]
          ]
