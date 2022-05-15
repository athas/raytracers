{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DataKinds #-}
module Image (mkImage, writeImage) where

import Data.Massiv.Array as A
import Data.Massiv.Array.IO

mkImage :: Int -> Int -> (Int -> Int -> Pixel (SRGB 'NonLinear) Word8) -> Image S (SRGB 'NonLinear) Word8
mkImage height width pixel =
  makeArrayR S Par (Sz2 height width) (\(i :. j) -> pixel i j)
