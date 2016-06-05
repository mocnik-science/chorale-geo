import Chorale.Geo.Test.Coordinates as ChoraleGeoTestCoordinates

import Test.Framework

main :: IO ()
main = defaultMainWithArgs testsToRun ["--maximum-generated-tests=1000"]

testsToRun :: [Test]
testsToRun = ChoraleGeoTestCoordinates.tests
