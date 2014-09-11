-- | QuickCheck's result type.
module Test.QuickCheck.Result where

import Test.QuickCheck.Random
import Test.QuickCheck.Exception

-- | Result represents the test result
data Result
  -- | A successful test run
  = Success
    { numTests       :: Int               -- ^ Number of tests performed
    , labels         :: [(String,Int)]    -- ^ Labels and frequencies found during all successful tests
    , output         :: String            -- ^ Printed output
    }
  -- | Given up
  | GaveUp
    { numTests       :: Int               --   Number of tests performed
    , labels         :: [(String,Int)]    --   Labels and frequencies found during all successful tests
    , output         :: String            --   Printed output
    }
  -- | A failed test run
  | Failure
    { numTests       :: Int               --   Number of tests performed
    , numShrinks     :: Int               -- ^ Number of successful shrinking steps performed
    , numShrinkTries :: Int               -- ^ Number of unsuccessful shrinking steps performed
    , numShrinkFinal :: Int               -- ^ Number of unsuccessful shrinking steps performed since last successful shrink
    , usedSeed       :: QCGen             -- ^ What seed was used
    , usedSize       :: Int               -- ^ What was the test size
    , reason         :: String            -- ^ Why did the property fail
    , theException   :: Maybe AnException -- ^ The exception the property threw, if any
    , labels         :: [(String,Int)]    --   Labels and frequencies found during all successful tests
    , output         :: String            --   Printed output
    }
  -- | A property that should have failed did not
  | NoExpectedFailure
    { numTests       :: Int               --   Number of tests performed
    , labels         :: [(String,Int)]    --   Labels and frequencies found during all successful tests
    , output         :: String            --   Printed output
    }
 deriving ( Show )
