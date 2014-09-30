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
import Trace.Hpc.Tix
import Data.IORef
import Control.Applicative

data Trace = Trace String


quickCheckWithHints :: Testable prop => prop -> IO ()
quickCheckWithHints p = quickCheckWithTrace stdArgs p >> return ()

quickCheckWithTrace :: Testable prop => Args -> prop -> IO (Result, Trace)
quickCheckWithTrace args p = do
  clearTix
  emptyTix <- examineTix
  successTix <- newIORef emptyTix
  failureTix <- newIORef emptyTix
  let args' = args { keepGoing = True }
      hooks = Hooks { preTestHook = preTest
                    , postTestHook = postTest }
      preTest :: State -> IO ()
      preTest st = clearTix
      postTest :: State -> P.Result -> IO ()
      postTest st res = case P.ok res of
                          Just True -> do
                                       updateTixRef successTix
                                       print "Success"                    
                          Just False -> do
                                        updateTixRef failureTix
                                        print "Failure"
                          
  res <- quickCheckWithHooks args' hooks p
  readIORef successTix >>= print
  readIORef failureTix >>= print
  return (res, Trace "foo")
        
-- Tix [TickModule "name" hash length-of-list [ticks]]

updateTixRef :: IORef Tix -> IO ()
updateTixRef tixref = do
  currentTix <- readIORef tixref
  newTix <- examineTix
  writeIORef tixref (addTix currentTix newTix)


addTix :: Tix -> Tix -> Tix
addTix (Tix modules) (Tix modules') = Tix $ zipWith addTix' modules modules'
    where
      addTix' (TixModule name hash len ticks) (TixModule name' _ len' ticks')
          | name /= name' || len /= len' = error "encountered incompatible Tix definitions!"
          | otherwise = TixModule name hash len ( zipWith (+) ticks ticks' )
