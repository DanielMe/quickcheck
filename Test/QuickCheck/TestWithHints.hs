-- | An addition to the standard quickCheck method which also keeps track
-- | of the coverage data to derive hints, where an error might have
-- | occurred
{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
module Test.QuickCheck.TestWithHints where
 
import Test.QuickCheck.Property hiding ( Result( reason, theException) )
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Test
import Test.QuickCheck.Result
import Test.QuickCheck.State
import Trace.Hpc.Reflect

data Trace = Trace String


quickCheckWithTrace :: Testable prop => Args -> prop -> IO (Result, Trace)
quickCheckWithTrace args p = do
  let args' = args { keepGoing = True }
      hooks = Hooks { preTestHook = preTest
                    , postTestHook = postTest }
      preTest :: State -> IO ()
      preTest st = return ()
      postTest :: State -> P.Result -> IO ()
      postTest st res = do
        tix <- examineTix
        putStrLn (show tix)
  res <- quickCheckWithHooks args' hooks p
  return (res, Trace "foo")
        
