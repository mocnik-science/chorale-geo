-----------------------------------------------------------------------------
-- |
-- Module      :  Chorale.Geo.Coordinates
-- Copyright   :  2014-2016 Franz-Benjamin Mocnik
-- License     :  MIT
--
-- Maintainer  :  mail@mocnik-science.net
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Chorale.Geo.Coordinates (
    -- * Constants
    radiusEarth,
    -- * Geo Functions
    degreeToRad,
    radToDegree,
    -- * Coordinates
    Coordinates(..),
    CoordinatesCartesian(..),
    CoordinatesWGS84(..),
    -- * Coordinate Transformations
    transformWGS84toCartesian) where

import Chorale.Common

import qualified Data.Binary as B
import GHC.Generics (Generic)

-- --== CONSTANTS

-- | radius of the Earth in km
radiusEarth :: Double
radiusEarth = 6371.009

-- --== GEO FUNCTIONS

-- | degree to rad
degreeToRad :: Double -> Double
degreeToRad d = d * pi / 180

-- | rad to degree
radToDegree :: Double -> Double
radToDegree r = r * 180 / pi

-- --== GEOMETRY

-- | norm of a vector
norm :: Floating a => [a] -> a
norm = sqrt . sum . uncurry (zipWith (*)) . make2

-- --== COORDINATES

-- | Coordinate class
class (Ord c, Eq c, Show c) => Coordinates c where
  toTuple :: c -> (Double, Double)
  distance :: c -> c -> Double
  azimuth :: c -> c -> Double

-- --== COORDINATES CARTESIAN

-- | Cartesian coordinates
newtype CoordinatesCartesian = CoordinatesCartesian (Double, Double) deriving (Ord, Eq, Show, Generic)

instance B.Binary CoordinatesCartesian

instance Coordinates CoordinatesCartesian where
    toTuple (CoordinatesCartesian x) = x
    distance = curry $ norm . uncurry (zipWith (-)) . map12 (tupleToList2 . toTuple)
    azimuth (CoordinatesCartesian (x1, y1)) (CoordinatesCartesian (x2, y2)) = alpha (x2 - x1, y2 - y1) where
        alpha (x, y)
            | x == 0 && y == 0 = error "azimuth cannot be computed between two identical vectors"
            | az < 0 = az + 2 * pi
            | otherwise = az where
                az = - atan2 y x + pi / 2

instance Coordinates (Double, Double) where
    toTuple = id
    distance = curry $ uncurry distance . map12 CoordinatesCartesian
    azimuth = curry $ uncurry azimuth . map12 CoordinatesCartesian

-- --== COORDINATES WGS84

-- | Geographic coordinates (WGS84)
newtype CoordinatesWGS84 = CoordinatesWGS84 (Double, Double) deriving (Ord, Eq, Show, Generic)

instance B.Binary CoordinatesWGS84

instance Coordinates CoordinatesWGS84 where
    toTuple (CoordinatesWGS84 x) = x
    distance cs1 cs2 = radiusEarth * sqrt (diffAngle lat2 lat1 ** 2 + (cos ((lat2 + lat1) / 2) * diffAngle lon2 lon1)**2) where
        [lat1, lon1] = map degreeToRad . tupleToList2 . toTuple $ cs1
        [lat2, lon2] = map degreeToRad . tupleToList2 . toTuple $ cs2
        diffAngle y x
            | abs (y - x) < pi / 2 = y - x
            | otherwise = diffAngle y (x + signum (y - x) * pi)
    -- Vincenty's formulae
    azimuth (CoordinatesWGS84 (lat1', lon1')) (CoordinatesWGS84 (lat2', lon2'))
        | lat1' - lat2' == 0 && lon1' - lon2' == 0 = error "azimuth cannot be computed between two identical vectors"
        | az < 0 = az + 2 * pi
        | otherwise = az where
            az = atan2 (cos u2 * sin lambda) (cos u1 * sin u2 - sin u1 * cos u2 * cos lambda)
            lat1 = degreeToRad lat1'
            lat2 = degreeToRad lat2'
            lon1 = degreeToRad lon1'
            lon2 = degreeToRad lon2'
            f = 1 / 298.257223563
            u1 = atan ((1 - f) * tan lat1)
            u2 = atan ((1 - f) * tan lat2)
            l = lon2 - lon1
            lambda = recursiveFunction l
            recursiveFunction x
                | abs (x - x') < 10e-12 = x'
                | otherwise = recursiveFunction x' where
                    sins = sqrt $ (cos u2 * sin x)**2 + (cos u1 * sin u2 - sin u1 * cos u2 * cos x)**2
                    coss = sin u1 * sin u2 + cos u1 * cos u2 * cos x
                    s = atan (sins / coss)
                    sina = (cos u1 * cos u2 * sin x) / sins
                    cos2a = 1 - sina**2
                    cos2sm = sqrt cos2a - 2 * sin u1 * sin u2 / cos2a
                    c = f / 16 * cos2a * (4 + f * (4 - 3 * cos2a))
                    x' = l + (1 - c) * f * sina * (s + c * sins * (cos2sm + c * coss * (-1 + 2 * cos2sm**2)))

-- --== COORDINATE TRANSFORMATIONS

-- | transform 'CoordinatesWGS84' to 'CoordinatesCartesian'
transformWGS84toCartesian :: Double -> CoordinatesWGS84 -> CoordinatesCartesian
transformWGS84toCartesian k (CoordinatesWGS84 (lat, lon)) = CoordinatesCartesian (128 / pi * 2**k * (degreeToRad lon + pi), 128 / pi * 2**k * (pi - log (tan (pi / 4 + degreeToRad lat / 2))))
