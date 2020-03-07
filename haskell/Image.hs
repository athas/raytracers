{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Image (Image(..), Pixel, image2ppm, mkImage) where

import Control.DeepSeq
import Control.Monad.Par
import qualified Data.Array as A
import Data.Word (Word8)
import GHC.Generics (Generic)

type Pixel = (Word8, Word8, Word8)

data Image =
  Image { imageWidth :: Int
        , imageHeight :: Int
        , imagePixels :: A.Array (Int, Int) Pixel
        -- ^ Row-major pixels.
        }
  deriving (Generic, NFData)

image2ppm :: Image -> String
image2ppm img =
  unlines $
  [ "P3"
  , show width ++ " " ++ show height
  , "255" ]
  ++ map pixel2ppm (A.elems (imagePixels img))
  where width = imageWidth img
        height = imageHeight img
        pixel2ppm (r, g, b) =
          show r ++ " " ++ show g ++ " " ++ show b

mkImage :: Int -> Int -> (Int -> Int -> Pixel) -> Image
mkImage width height pixel =
  Image width height $ A.listArray ((0,0), (height-1, width-1)) $
  runPar $ parMap id
  [ pixel j i | j <- [height-1,height-2..0], i <- [0..width-1] ]
