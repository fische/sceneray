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

shapes :: [ColoredShape Sphere]
shapes = [
  ColoredShape (Sphere (Point (-1) 0 (-10)) 4) (PixelRGBA16 0x00FF 0x00FF 0xFFFF 0xFFFF),
  ColoredShape (Sphere (Point 1 0 (-10)) 4) (PixelRGBA16 0xFFFF 0x00FF 0x00FF 0xFFFF)]

-- Types
class Shape s where
  intersect :: s -> Ray -> Bool
  intersections :: s -> Ray -> [Point]
  color :: s -> PixelRGBA16
  color _ = PixelRGBA16 0xFFFF 0xFFFF 0xFFFF 0xFFFF

data Sphere = Sphere
  { centre :: Point,
    radius :: Double
  }
  deriving (Show)

data (Shape s) => ColoredShape s = ColoredShape s PixelRGBA16 deriving (Show)

data Ray = Ray Point Vector deriving (Show)

data Vector = Vector Double Double Double deriving (Show)

data Point = Point Double Double Double deriving (Show, Eq)

type Distance = Double

-- Functions
instance Ord Point where
  compare (Point ax ay az) (Point bx by bz) =
    compare da db
    where da = ax * ax + ay * ay + az * az
          db = bx * bx + by * by + bz * bz

instance Shape Sphere where
  intersect s (Ray (Point {}) (Vector vx vy vz)) =
    (b * b - 4 * a * c) >= 0
    where
      r = radius s
      Point cx cy cz = centre s
      a = vx * vx + vy * vy + vz * vz
      b = 2 * (vx * cx + vy * cy + vz * cz)
      c = cx * cx + cy * cy + cz * cz - r * r
  intersections s (Ray (Point ox oy oz) (Vector vx vy vz))
    | delta > 0 =
        let sq = sqrt (b *b - 4 * a * c)
	    x1 = ((-1) * b - sq) / (2 * a)
	    x2 = ((-1) * b + sq) / (2 * a)
	 in [Point (vx * x1 + ox) (vy * x1 + oy) (vz * x1 + oz), Point (vx * x2 + ox) (vy * x2 + oy) (vz * x2 + oz)]
    | delta == 0 =
        let x = (-1) * b / (2 * a)
         in [Point (vx * x + ox) (vy * x + oy) (vz * x + oz)]
    | otherwise = []
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

translate :: Vector -> Point -> Point
translate (Vector vx vy vz) (Point px py pz) = Point (px + vx) (py + vy) (pz + vz) 

-- NOTE: be careful, points are translated using origin of Ray as origin
closest' :: (Shape s) => Ray -> s -> Maybe (s, Point) -> Maybe (s, Point)
closest' r@(Ray (Point ox oy oz) _) s v =
  case intersections s r of
    [] -> v
    i -> case v of
      Nothing -> Just (s, cp)
      Just (_, cp1)
        | cp < cp1 -> Just (s, cp)
        | otherwise -> v
      where (p:l) = map (translate $ Vector (-1 * ox) (-1 * oy) (-1 * oz)) i
            cp = foldr min p l

closest :: (Shape s) => Ray -> [s] -> Maybe s
closest r l =
  case foldr (closest' r) Nothing l of
    Nothing -> Nothing
    Just (s, _) -> Just s

viewPortX :: Int -> Double
viewPortX x = (2 * ((fromIntegral x + 0.5) / fromIntegral sizeX) - 1) * aspectRatio * viewPortUnit

viewPortY :: Int -> Double
viewPortY y = (1 - 2 * ((fromIntegral y + 0.5) / fromIntegral sizeY)) * viewPortUnit

vectorFromPixelPoint :: Int -> Int -> Vector
vectorFromPixelPoint x y = Vector (viewPortX x) (viewPortY y) (-1)

pixelRenderer :: (Shape s) => [s] -> Int -> Int -> PixelRGBA16
pixelRenderer s x y =
  maybe (PixelRGBA16 0 0 0 0xFFFF) color $
    closest (Ray (Point 0 0 0) (vectorFromPixelPoint x y)) s

newImage :: Image PixelRGBA16
newImage = generateImage (pixelRenderer shapes) sizeX sizeY

main :: IO ()
main = writePng "/home/fische/Projects/sceneray/test.png" newImage
