module Main where

import Codec.Picture.Png
import Codec.Picture.Types

-- Constants
-- TODO allow setting camera origin
-- TODO allow setting camera direction
sizeX :: Int
sizeX = 1920

sizeY :: Int
sizeY = 1080

aspectRatio :: Double
aspectRatio = fromIntegral sizeX / fromIntegral sizeY

fov :: Int
fov = 110

viewPortUnit :: Double
viewPortUnit = tan (fromIntegral fov / 2 * pi / 180)

shapes :: ColoredShape Sphere
shapes = ColoredShape (Sphere (Point 0 0 (-10)) 4) (PixelRGBA16 0x00FF 0x00FF 0xFFFF 0xFFFF)

-- Types
class Shape s where
  intersect :: s -> Line -> Bool
  intersections :: s -> Line -> Maybe (Either Point (Point, Point))
  color :: s -> PixelRGBA16
  color _ = PixelRGBA16 0xFFFF 0xFFFF 0xFFFF 0xFFFF

data Sphere = Sphere
  { centre :: Point,
    radius :: Double
  }
  deriving (Show)

data (Shape s) => ColoredShape s = ColoredShape s PixelRGBA16 deriving (Show)

data Line = Line Point Vector deriving (Show)

data Vector = Vector Double Double Double deriving (Show)

data Point = Point Double Double Double deriving (Show)

data Pixel = Pixel Int Int deriving (Show)

-- Functions
viewPortX :: Int -> Double
viewPortX x = (2 * ((fromIntegral x + 0.5) / fromIntegral sizeX) - 1) * aspectRatio * viewPortUnit

viewPortY :: Int -> Double
viewPortY y = (1 - 2 * ((fromIntegral y + 0.5) / fromIntegral sizeY)) * viewPortUnit

instance Shape Sphere where
  intersect s (Line (Point {}) (Vector vx vy vz)) =
    (b * b - 4 * a * c) >= 0
    where
      r = radius s
      Point cx cy cz = centre s
      a = vx * vx + vy * vy + vz * vz
      b = 2 * (vx * cx + vy * cy + vz * cz)
      c = cx * cx + cy * cy + cz * cz - r * r
  intersections s (Line (Point ox oy oz) (Vector vx vy vz))
    | delta > 0 = Nothing
    | delta == 0 =
        let t = (-1) * b / (2 * a)
            p = Point (vx * t + ox) (vy * t + oy) (vz * t + oz)
         in Just $ Left p
    | otherwise = Nothing
    where
      r = radius s
      Point cx cy cz = centre s
      ocx = cx + ox
      ocy = cy + oy
      ocz = cz + oz
      a = vx * vx + vy * vy + vz * vz
      b = 2 * (vx * ocx + vy * ocy + vz * ocz)
      c = ocx * ocx + ocy * ocy + ocz * ocz - r * r
      delta = b * b - 4 * a * c

instance (Shape s) => Shape (ColoredShape s) where
  intersect (ColoredShape s _) = intersect s
  intersections (ColoredShape s _) = intersections s
  color (ColoredShape _ c) = c

rayFromPixelPoint :: Int -> Int -> Vector
rayFromPixelPoint x y = Vector (viewPortX x) (viewPortY y) (-1)

pixelRenderer :: (Shape s) => s -> Int -> Int -> PixelRGBA16
pixelRenderer s x y
  | intersect s $ Line (Point 0 0 0) (rayFromPixelPoint x y) = color s
  | otherwise = PixelRGBA16 0 0 0 0xFFFF

newImage :: Image PixelRGBA16
newImage = generateImage (pixelRenderer shapes) sizeX sizeY

main :: IO ()
main = writePng "/home/fische/Projects/sceneray/test.png" newImage
