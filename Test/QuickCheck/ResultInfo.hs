module Test.QuickCheck.ResultInfo where

import Test.QuickCheck.Random
import Test.QuickCheck.Exception

data ResultInfo
  -- | A successful test run
  = SuccessInfo
    { numTests       :: Int               -- ^ Number of tests performed
    }
  -- | Given up
  | GaveUpInfo
    { numTests       :: Int               --   Number of tests performed
    }
  -- | A failed test run
  | FailureInfo
    { numTests       :: Int               --   Number of tests performed
    , numShrinks     :: Int               -- ^ Number of successful shrinking steps performed
    , numShrinkTries :: Int               -- ^ Number of unsuccessful shrinking steps performed
    , numShrinkFinal :: Int               -- ^ Number of unsuccessful shrinking steps performed since last successful shrink
    , usedSeed       :: QCGen             -- ^ What seed was used
    , usedSize       :: Int               -- ^ What was the test size
    , reason         :: String            -- ^ Why did the property fail
    , theException   :: Maybe AnException -- ^ The exception the property threw, if any
    }
  -- | A property that should have failed did not
  | NoExpectedFailureInfo
    { numTests       :: Int               --   Number of tests performed
    }
 deriving ( Show )


                               
