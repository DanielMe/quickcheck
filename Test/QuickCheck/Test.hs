-- | The main test loop.
{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
module Test.QuickCheck.Test where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Property hiding ( Result( reason, theException) )
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Text
import Test.QuickCheck.State
import Test.QuickCheck.Exception
import Test.QuickCheck.Random
import Test.QuickCheck.Result
import qualified Test.QuickCheck.ResultInfo as RI

import System.Random(split)

import Data.Char
  ( isSpace
  )

import Data.List
  ( sort
  , group
  , groupBy
  , intersperse
  )
--------------------------------------------------------------------------
-- quickCheck

-- * Running tests

-- | Args specifies arguments to the QuickCheck driver
data Args
  = Args
  { replay          :: Maybe (QCGen,Int) -- ^ Should we replay a previous test?
  , maxSuccess      :: Int               -- ^ Maximum number of successful tests before succeeding
  , maxDiscardRatio :: Int               -- ^ Maximum number of discarded tests per successful test before giving up
  , maxSize         :: Int               -- ^ Size to use for the biggest test cases
  , chatty          :: Bool              -- ^ Whether to print anything
  , keepGoing       :: Bool              -- ^ Whether we should abort on the first counterexample or just continue
  }
 deriving ( Show, Read )

-- | Callbacks to hook into certain states of the test procedure with custom functions
data Hooks = Hooks 
    { preTestHook     :: State -> IO ()              -- ^ Execute this right before a test
    , postTestHook    :: State -> P.Result -> IO ()              -- ^ Execute this right after a test
    }



-- | Check if the test run result was a success
isSuccess :: Result -> Bool
isSuccess Success{} = True
isSuccess _         = False

-- | The default test arguments
stdArgs :: Args
stdArgs = Args
  { replay          = Nothing
  , maxSuccess      = 100
  , maxDiscardRatio = 10
  , maxSize         = 100
  , chatty          = True
  , keepGoing       = False
-- noShrinking flag?
  }

stdHooks :: Hooks
stdHooks = Hooks 
  { preTestHook = const $ return ()
  , postTestHook = const $ const $ return ()
  }

-- | Tests a property and prints the results to 'stdout'.
quickCheck :: Testable prop => prop -> IO ()
quickCheck p = quickCheckWith stdArgs p

-- | Tests a property, using test arguments, and prints the results to 'stdout'.
quickCheckWith :: Testable prop => Args -> prop -> IO ()
quickCheckWith args p = quickCheckWithResult args p >> return ()

-- | Tests a property, produces a test result, and prints the results to 'stdout'.
quickCheckResult :: Testable prop => prop -> IO Result
quickCheckResult p = quickCheckWithResult stdArgs p

-- | Tests a property, using test arguments, produces a test result, and prints the results to 'stdout'.
quickCheckWithResult :: Testable prop => Args -> prop -> IO Result
quickCheckWithResult a p = quickCheckWithHooks a stdHooks p

-- | Tests a property, using test arguments and callback hooks, produces a test result, and prints the results to 'stdout'.
quickCheckWithHooks :: Testable prop => Args -> Hooks -> prop -> IO Result
quickCheckWithHooks a hooks p = (if chatty a then withStdioTerminal else withNullTerminal) $ \tm -> do
     rnd <- case replay a of
              Nothing      -> newQCGen
              Just (rnd,_) -> return rnd
     finalState <- test a hooks (initialState a tm rnd) (unGen (unProperty (property' p)))
     doneTesting finalState
  where 
    property' p
        | exhaustive p = once (property p)
        | otherwise = property p

-- | Constructs a "blank" initial state from a given set of attributes, a terminal and a random seed
initialState a tm rnd = MkState{ terminal                  = tm
                               , maxSuccessTests           = maxSuccess a
                               , maxDiscardedTests         = maxDiscardRatio a * maxSuccess a
                               , maxFailedTests            = maxSuccess a -- TODO: well, instead of "keepGoing" we might want to have an exact number in the arguments?
                               , computeSize               = case replay a of
                                                               Nothing    -> computeSize'
                                                               Just (_,s) -> computeSize' `at0` s
                               , numSuccessTests           = 0
                               , numFailedTests            = 0
                               , numDiscardedTests         = 0
                               , numRecentlyDiscardedTests = 0
                               , collected                 = []
                               , expectedFailure           = False
                               , randomSeed                = rnd
                               , numSuccessShrinks         = 0
                               , numTryShrinks             = 0
                               , numTotTryShrinks          = 0
                               , finalResult               = Nothing
                               }
    where 
      at0 f s 0 0 = s
      at0 f s n d = f n d
      n `roundTo` m = (n `div` m) * m
      computeSize' n d
          -- e.g. with maxSuccess = 250, maxSize = 100, goes like this:
          -- 0, 1, 2, ..., 99, 0, 1, 2, ..., 99, 0, 2, 4, ..., 98.
          | n `roundTo` maxSize a + maxSize a <= maxSuccess a ||
            n >= maxSuccess a ||
            maxSuccess a `mod` maxSize a == 0 = (n `mod` maxSize a + d `div` 10) `min` maxSize a
          | otherwise =
            ((n `mod` maxSize a) * maxSize a `div` (maxSuccess a `mod` maxSize a) + d `div` 10) `min` maxSize a


-- | Tests a property and prints the results and all test cases generated to 'stdout'.
-- This is just a convenience function that means the same as @'quickCheck' . 'verbose'@.
verboseCheck :: Testable prop => prop -> IO ()
verboseCheck p = quickCheck (verbose p)

-- | Tests a property, using test arguments, and prints the results and all test cases generated to 'stdout'.
-- This is just a convenience function that combines 'quickCheckWith' and 'verbose'.
verboseCheckWith :: Testable prop => Args -> prop -> IO ()
verboseCheckWith args p = quickCheckWith args (verbose p)

-- | Tests a property, produces a test result, and prints the results and all test cases generated to 'stdout'.
-- This is just a convenience function that combines 'quickCheckResult' and 'verbose'.
verboseCheckResult :: Testable prop => prop -> IO Result
verboseCheckResult p = quickCheckResult (verbose p)

-- | Tests a property, using test arguments, produces a test result, and prints the results and all test cases generated to 'stdout'.
-- This is just a convenience function that combines 'quickCheckWithResult' and 'verbose'.
verboseCheckWithResult :: Testable prop => Args -> prop -> IO Result
verboseCheckWithResult a p = quickCheckWithResult a (verbose p)

--------------------------------------------------------------------------
-- main test loop

test :: Args -> Hooks -> State -> (QCGen -> Int -> Prop) -> IO State
test args hooks st f
  | numSuccessTests st   >= maxSuccessTests st   = return $ maybeSetResult st (RI.SuccessInfo $ numSuccessTests st)
  | numFailedTests st   >= maxFailedTests st   = return $ maybeSetResult st (RI.SuccessInfo $ numSuccessTests st) -- TODO: there should already be something in finalResult... have to think about this...
  | numDiscardedTests st >= maxDiscardedTests st = return $ maybeSetResult st (RI.GaveUpInfo $ numSuccessTests st)
  | otherwise                                   = runATest args hooks st f


doneTesting :: State -> IO Result
doneTesting st = do showMessage
                    runSuccessHook
                    theOutput <- terminalOutput (terminal st)
                    return (result theOutput)
    where
      Just res = finalResult st
      showMessage = case res of
                      RI.SuccessInfo{} -> showSuccessMessage
                      RI.FailureInfo{} -> showFailureMessage
                      RI.GaveUpInfo{} -> showGaveUpMessage
                      RI.NoExpectedFailureInfo{} -> showNoExpectedFailureMessage
      showSuccessMessage = putPart (terminal st)("+++ OK, passed " ++ show (numSuccessTests st) ++ " tests, failed " ++ show (numFailedTests st) ++ " tests")
      showFailureMessage = putPart (terminal st)( bold ("*** Failed!")++ " Passed " ++ show (numSuccessTests st) ++ " tests, failed " ++ show (numFailedTests st) ++ " tests")
      showGaveUpMessage = putPart (terminal st)( bold ("*** Gave up!")++ " Passed only"++ show (numSuccessTests st)++ " tests, failed " ++ show (numFailedTests st) ++ " tests")
      showNoExpectedFailureMessage = putPart (terminal st)( bold ("*** Failed!")++ " Passed "++ show (numSuccessTests st)++ " tests (expected failure)" )
      runSuccessHook = case res of
                      RI.SuccessInfo{} -> success st
                      RI.FailureInfo{} -> return ()
                      RI.GaveUpInfo{} -> success st
                      RI.NoExpectedFailureInfo{} -> return ()
      result theOutput = case res of
                      (RI.SuccessInfo n) -> Success { labels = summary st,
                                                     numTests = n,
                                                     output = theOutput }
                      (RI.GaveUpInfo n) -> GaveUp { labels = summary st,
                                                   numTests = n,
                                                   output = theOutput }
                      (RI.NoExpectedFailureInfo n) -> NoExpectedFailure { labels = summary st,
                                                                         numTests = n,
                                                                         output = theOutput }
                      (RI.FailureInfo numTests numShrinks numShrinkTries numShrinkFinal usedSeed usedSize reason theException) ->
                          Failure {labels = summary st,
                                   output = theOutput,
                                   numTests = numTests,
                                   numShrinks = numShrinks,
                                   numShrinkTries = numShrinkTries,
                                   numShrinkFinal = numShrinkFinal,
                                   usedSeed = usedSeed,
                                   usedSize = usedSize,
                                   reason = reason,
                                   theException = theException}

type ResultHandler = Args -> Hooks -> P.Result -> [Rose P.Result] -> State -> (QCGen -> Int -> Prop) -> IO State

maybeSetResult :: State -> RI.ResultInfo -> State
maybeSetResult st res = maybe st{finalResult = Just res} (const st) (finalResult st)

handleSuccessResult :: ResultHandler
handleSuccessResult args hooks MkResult{abort = abort, stamp = stamp, expect = expect} _ st f
    | abort = return $ maybeSetResult st successResult
    | otherwise  = test args hooks st' f -- continue
    where
      successResult
       | expect = RI.SuccessInfo $ numSuccessTests st
       | otherwise = RI.NoExpectedFailureInfo $ numSuccessTests st
      st' = st{ numSuccessTests           = numSuccessTests st + 1
              , numRecentlyDiscardedTests = 0
              , randomSeed                = nextSeed st
              , collected                 = stamp : (collected st)
              , expectedFailure           = expect
              }
      
handleDiscardedResult :: ResultHandler
handleDiscardedResult args hooks MkResult{expect = expect, abort = abort} _ st f
    | abort     = return $ maybeSetResult st (RI.GaveUpInfo $ numSuccessTests st)
    | otherwise = test args hooks st' f --continue
    where
      st' = st{ numDiscardedTests         = numDiscardedTests st + 1
              , numRecentlyDiscardedTests = numRecentlyDiscardedTests st + 1
              , randomSeed                = nextSeed st
              , expectedFailure           = expect
              }

handleFailedResult :: ResultHandler
handleFailedResult args hooks res@(MkResult{abort = abort, stamp = stamp, expect = expect}) ts st f
    | (not abort) && (keepGoing args)  = do 
                                      res' <- failureResult
                                      putPart (terminal st) (bold "*** ... keep going! ")
                                      test args hooks (nextState res') f
    | otherwise = do 
                  res' <- failureResult
                  return $ maybeSetResult st res'
    where
      size = computeSize st (numSuccessTests st + numFailedTests st) (numRecentlyDiscardedTests st)
      failureResult  
          | expect = do 
                     putPart (terminal st) (bold "*** Failure detected! ")
                     (numShrinks, totFailed, lastFailed) <- foundFailure hooks st res ts
                     return RI.FailureInfo{ RI.usedSeed = randomSeed st
                                          , RI.usedSize       = size
                                          , RI.numTests       = numSuccessTests st+1
                                          , RI.numShrinks     = numShrinks
                                          , RI.numShrinkTries = totFailed
                                          , RI.numShrinkFinal = lastFailed
                                          , RI.reason         = P.reason res
                                          , RI.theException   = P.theException res
                                          }
          | otherwise = do
                        putPart (terminal st) (bold "+++ OK, failed as expected! ")
                        return $ RI.SuccessInfo $ numSuccessTests st + 1
      nextState finalRes = st{ numFailedTests           = numFailedTests st + 1
                             , numRecentlyDiscardedTests = 0
                             , randomSeed                = nextSeed st
                             , collected                 = stamp : (collected st)
                             , expectedFailure           = expect
                             , finalResult               = maybe (Just finalRes) Just (finalResult st) 
                             }

handleResult :: ResultHandler
handleResult args hooks res@(MkResult{ok = Just True}) = handleSuccessResult args hooks res
handleResult args hooks res@(MkResult{ok = Nothing}) = handleDiscardedResult args hooks res
handleResult args hooks res@(MkResult{ok = Just False}) = handleFailedResult args hooks res

runATest :: Args -> Hooks -> State -> (QCGen -> Int -> Prop) -> IO State
runATest args hooks st f =
  do -- CALLBACK before_test
     putTemp (terminal st)
        ( "("
       ++ number (numSuccessTests st) "test"
       ++ concat [ "; " ++ show (numDiscardedTests st) ++ " discarded"
                 | numDiscardedTests st > 0
                 ]
       ++ ")"
        )
     let size = computeSize st (numSuccessTests st + numFailedTests st) (numRecentlyDiscardedTests st)
     callbackPreTest hooks st
     MkRose res ts <- protectRose (reduceRose (unProp (f rnd1 size)))
     callbackPostTest hooks st res
     finalState <- handleResult args hooks res ts st f
     return finalState
 where
  (rnd1,rnd2) = split (randomSeed st)


nextSeed = snd . split . randomSeed

summary :: State -> [(String,Int)]
summary st = reverse
           . sort
           . map (\ss -> (head ss, (length ss * 100) `div` numSuccessTests st))
           . group
           . sort
           $ [ concat (intersperse ", " s')
             | s <- collected st
             , let s' = [ t | (t,_) <- s ]
             , not (null s')
             ]

success :: State -> IO ()
success st =
  case allLabels ++ covers of
    []    -> do putLine (terminal st) "."
    [pt]  -> do putLine (terminal st)
                  ( " ("
                 ++ dropWhile isSpace pt
                 ++ ")."
                  )
    cases -> do putLine (terminal st) ":"
                sequence_ [ putLine (terminal st) pt | pt <- cases ]
 where
  allLabels = reverse
            . sort
            . map (\ss -> (showP ((length ss * 100) `div` numSuccessTests st) ++ head ss))
            . group
            . sort
            $ [ concat (intersperse ", " s')
              | s <- collected st
              , let s' = [ t | (t,0) <- s ]
              , not (null s')
              ]

  covers = [ ("only " ++ show occurP ++ "% " ++ fst (head lps) ++ "; not " ++ show reqP ++ "%")
           | lps <- groupBy first
                  . sort
                  $ [ lp
                    | lps <- collected st
                    , lp <- maxi lps
                    , snd lp > 0
                    ]
           , let occurP = (100 * length lps) `div` maxSuccessTests st
                 reqP   = maximum (map snd lps)
           , occurP < reqP
           ]

  (x,_) `first` (y,_) = x == y

  maxi = map (\lps -> (fst (head lps), maximum (map snd lps)))
       . groupBy first
       . sort

  showP p = (if p < 10 then " " else "") ++ show p ++ "% "

--------------------------------------------------------------------------
-- main shrinking loop

foundFailure :: Hooks -> State -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int)
foundFailure hooks st res ts =
  do localMin hooks st{ numTryShrinks = 0 } res res ts

localMin :: Hooks -> State -> P.Result -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int)
localMin hooks st MkResult{P.theException = Just e} lastRes _
  | isInterrupt e = localMinFound st lastRes
localMin hooks st res _ ts = do
  putTemp (terminal st)
    ( short 26 (oneLine (P.reason res))
   ++ " (after " ++ number (numSuccessTests st+1) "test"
   ++ concat [ " and "
            ++ show (numSuccessShrinks st)
            ++ concat [ "." ++ show (numTryShrinks st) | numTryShrinks st > 0 ]
            ++ " shrink"
            ++ (if numSuccessShrinks st == 1
                && numTryShrinks st == 0
                then "" else "s")
             | numSuccessShrinks st > 0 || numTryShrinks st > 0
             ]
   ++ ")..."
    )
  r <- tryEvaluate ts
  case r of
    Left err ->
      localMinFound st
         (exception "Exception while generating shrink-list" err) { callbacks = callbacks res }
    Right ts' -> localMin' hooks st res ts'

localMin' :: Hooks -> State -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int)
localMin' _ st res [] = localMinFound st res
localMin' hooks st res (t:ts) =
  do -- CALLBACK before_test
    callbackPreTest hooks st
    MkRose res' ts' <- protectRose (reduceRose t)
    callbackPostTest hooks st res'
    if ok res' == Just False
      then localMin hooks st{ numSuccessShrinks = numSuccessShrinks st + 1,
                        numTryShrinks     = 0 } res' res ts'
      else localMin hooks st{ numTryShrinks    = numTryShrinks st + 1,
                        numTotTryShrinks = numTotTryShrinks st + 1 } res res ts

localMinFound :: State -> P.Result -> IO (Int, Int, Int)
localMinFound st res =
  do let report = concat [
           "(after " ++ number (numSuccessTests st+1) "test",
           concat [ " and " ++ number (numSuccessShrinks st) "shrink"
                  | numSuccessShrinks st > 0
                  ],
           "): "
           ]
     if isOneLine (P.reason res)
       then putLine (terminal st) (P.reason res ++ " " ++ report)
       else do
         putLine (terminal st) report
         sequence_
           [ putLine (terminal st) msg
           | msg <- lines (P.reason res)
           ]
     callbackPostFinalFailure st res
     return (numSuccessShrinks st, numTotTryShrinks st - numTryShrinks st, numTryShrinks st)

--------------------------------------------------------------------------
-- callbacks

callbackPostTest :: Hooks -> State -> P.Result -> IO ()
callbackPostTest hooks st res = do
  let hook = postTestHook hooks
  safely st (hook st res)
  sequence_ [ safely st (f st res) | PostTest _ f <- callbacks res ]

callbackPreTest :: Hooks -> State -> IO ()
callbackPreTest hooks st = 
    let hook = preTestHook hooks
    in  safely st (hook st)

callbackPostFinalFailure :: State -> P.Result -> IO ()
callbackPostFinalFailure st res =
  sequence_ [ safely st (f st res) | PostFinalFailure _ f <- callbacks res ]

safely :: State -> IO () -> IO ()
safely st x = do
  r <- tryEvaluateIO x
  case r of
    Left e ->
      putLine (terminal st)
        ("*** Exception in callback: " ++ show e)
    Right x ->
      return x

--------------------------------------------------------------------------
-- the end.
