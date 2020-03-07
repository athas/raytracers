module Main (main) where

import Criterion.Main
import Raytracing
import Scene

main :: IO ()
main = defaultMain
  [ bgroup "bvh"
    [ bench "rgbbox" $ bvhbench rgbbox
    , bench "irreg" $ bvhbench irreg
    ]

  , bgroup "rendering"
    [bgroup "rgbbox" $
     [ bench "10x10" $ renderbench rgbbox 10 10
     , bench "100x100" $ renderbench rgbbox 100 100
     , bench "200x200" $ renderbench rgbbox 200 200
     , bench "1000x1000" $ renderbench rgbbox 1000 1000
     ]
    , bgroup "irreg" $
      [ bench "10x10" $ renderbench irreg 10 10
      , bench "100x100" $ renderbench irreg 100 100
      , bench "200x200" $ renderbench irreg 200 200
      , bench "1000x1000" $ renderbench irreg 1000 1000
      ]
    ]
  ]
  where bvhbench scene =
          nf (fst . fromScene 1 1) scene

        renderbench scene width height =
          let (objs, cam) = fromScene width height scene
          in nf (render objs width height) cam
