module Main where

import Codec.Picture.Types
import Codec.Picture.Png

-- Types
data Sphere = Sphere {
	centre :: Point,
	radius :: Double
} deriving (Show)

data Vector = Vector Double Double Double deriving (Show)

data Point = Point Double Double Double deriving (Show)

data Pixel = Pixel Int Int deriving (Show)

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

-- Objects
sphere :: Sphere
sphere = Sphere (Point 0 0 (-10)) 4

-- Functions
viewPortX :: Int -> Double
viewPortX x = (2 * ((fromIntegral x + 0.5) / fromIntegral sizeX) - 1) * aspectRatio * viewPortUnit

viewPortY :: Int -> Double
viewPortY y = (1 - 2 * ((fromIntegral y + 0.5) / fromIntegral sizeY)) * viewPortUnit

rayFromPixelPoint :: Int -> Int -> Vector
rayFromPixelPoint x y = Vector (viewPortX x) (viewPortY y) (-1)

intersectSphere :: Sphere -> Vector -> Bool
intersectSphere s (Vector vx vy vz) =
	(b * b - 4 * a * c) >= 0 where
		r = radius s
		Point cx cy cz = centre s
		a = vx * vx + vy * vy + vz * vz
		b = 2 * (vx * cx + vy * cy + vz * cz)
		c = cx * cx + cy * cy + cz * cz - r * r


pixelRenderer :: Int -> Int -> PixelRGBA16
pixelRenderer x y | intersectSphere sphere (rayFromPixelPoint x y) = PixelRGBA16 0xFFFF 0xFFFF 0xFFFF 0xFFFF
                  | otherwise = PixelRGBA16 0 0 0 0xFFFF

newImage :: Image PixelRGBA16
newImage = generateImage pixelRenderer sizeX sizeY

main :: IO ()
main = writePng "/home/fische/Projects/sceneray/test.png" $ newImage
