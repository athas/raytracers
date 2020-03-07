module Main (main) where

import System.Environment (getArgs)
import Text.Read
import Image
import Raytracing
import Scene

scenes :: [(String, Scene)]
scenes = [ ("rgbbox", rgbbox)
         , ("irreg", irreg)
         ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [scene, width, height]
      | Just width' <- readMaybe width,
        Just height' <- readMaybe height -> do
          case lookup scene scenes of
            Nothing -> error $ "Invalid scene.  Known scenes:\n" ++ unlines (map fst scenes)
            Just scene' -> do
              let (objs, cam) = fromScene width' height' scene'
              putStr $ image2ppm $ render objs width' height' cam
    _ ->
      error $ "Usage: render <scene> <width> <height>"
