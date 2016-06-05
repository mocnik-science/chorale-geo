module Chorale.Geo.Test.Coordinates (
    tests) where

import Chorale.Common
import Chorale.Geo.Coordinates
import Chorale.Geo.Test

import Test.Framework hiding (testGroup)
--import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
--import Test.HUnit hiding (Test)
import Test.QuickCheck.Arbitrary
--import Test.QuickCheck.Property

instance Arbitrary CoordinatesCartesian where
    arbitrary = fmap (CoordinatesCartesian) arbitrary
    shrink (CoordinatesCartesian x) = map CoordinatesCartesian . shrink $ x

instance Arbitrary CoordinatesWGS84 where
    arbitrary = CoordinatesWGS84 <$> sequence2 (arbitraryInInterval 90, arbitraryInInterval 180) where
        arbitraryInInterval i = do
            x <- arbitrary
            let x' = x - (fromInteger . floor) x
            return $ if x' > 0.5
            then i * 2 * (x' - 1)
            else i * 2 * x' where
    shrink (CoordinatesWGS84 x) = map CoordinatesWGS84 . shrink $ x

-- --== Tests

tests :: [Test]
tests = [testDefiniteForm, testNonNegativity, testSymmetry, testTriangleEquation]

-- --== Definite Form

testDefiniteForm :: Test
testDefiniteForm = testGroup "Definite Form" [
        testProperty "cartesian coordinates" propDefiniteFormCartesian,
        testProperty "geographic coordinates (WGS84)" propDefiniteFormWGS84
    ]

propDefiniteFormCartesian :: CoordinatesCartesian -> Bool
propDefiniteFormCartesian x = distance x x == 0

propDefiniteFormWGS84 :: CoordinatesCartesian -> Bool
propDefiniteFormWGS84 x = distance x x == 0

-- --== Non-Negativity

testNonNegativity :: Test
testNonNegativity = testGroup "Non-Negativity" [
        testProperty "cartesian coordinates" propNonNegativityCartesian,
        testProperty "geographic coordinates (WGS84)" propNonNegativityWGS84
    ]

propNonNegativityCartesian :: CoordinatesCartesian -> CoordinatesCartesian -> Bool
propNonNegativityCartesian x y = distance x y >= 0

propNonNegativityWGS84 :: CoordinatesWGS84 -> CoordinatesWGS84 -> Bool
propNonNegativityWGS84 x y = distance x y >= 0

-- --== Symmetry

testSymmetry :: Test
testSymmetry = testGroup "Symmetry" [
        testProperty "cartesian coordinates" propSymmetryCartesian,
        testProperty "geographic coordinates (WGS84)" propSymmetryWGS84
    ]

propSymmetryCartesian :: CoordinatesCartesian -> CoordinatesCartesian -> Bool
propSymmetryCartesian x y = distance x y == distance y x

propSymmetryWGS84 :: CoordinatesCartesian -> CoordinatesCartesian -> Bool
propSymmetryWGS84 x y = distance x y == distance y x

-- --== Triangle Equation

testTriangleEquation :: Test
testTriangleEquation = testGroup "Triangle Equation" [
        testProperty "cartesian coordinates" propTriangleQuationCartesian
    ]

propTriangleQuationCartesian :: CoordinatesCartesian -> CoordinatesCartesian -> CoordinatesCartesian -> Bool
propTriangleQuationCartesian x y z = distance x y + distance y z >= distance x z
