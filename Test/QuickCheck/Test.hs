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
     test a hooks MkState{ terminal                  = tm
                         , maxSuccessTests           = maxSuccess a
                         , maxDiscardedTests         = maxDiscardRatio a * maxSuccess a
                         , computeSize               = case replay a of
                                                         Nothing    -> computeSize'
                                                         Just (_,s) -> computeSize' `at0` s
                         , numSuccessTests           = 0
                         , numDiscardedTests         = 0
                         , numRecentlyDiscardedTests = 0
                         , collected                 = []
                         , expectedFailure           = False
                         , randomSeed                = rnd
                         , numSuccessShrinks         = 0
                         , numTryShrinks             = 0
                         , numTotTryShrinks          = 0
                         , finalResult               = Nothing
                         } (unGen (unProperty (property' p)))
  where computeSize' n d
          -- e.g. with maxSuccess = 250, maxSize = 100, goes like this:
          -- 0, 1, 2, ..., 99, 0, 1, 2, ..., 99, 0, 2, 4, ..., 98.
          | n `roundTo` maxSize a + maxSize a <= maxSuccess a ||
            n >= maxSuccess a ||
            maxSuccess a `mod` maxSize a == 0 = (n `mod` maxSize a + d `div` 10) `min` maxSize a
          | otherwise =
            ((n `mod` maxSize a) * maxSize a `div` (maxSuccess a `mod` maxSize a) + d `div` 10) `min` maxSize a
        n `roundTo` m = (n `div` m) * m
        at0 f s 0 0 = s
        at0 f s n d = f n d
        property' p
          | exhaustive p = once (property p)
          | otherwise = property p

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

test :: Args -> Hooks -> State -> (QCGen -> Int -> Prop) -> IO Result
test args hooks st f
  | numSuccessTests st   >= maxSuccessTests st   = doneTesting st f
  | numDiscardedTests st >= maxDiscardedTests st = giveUp st f
  | otherwise                                    = runATest args hooks st f

doneTesting :: State -> (QCGen -> Int -> Prop) -> IO Result
doneTesting st _f =
  do -- CALLBACK done_testing?
     if expectedFailure st then
       putPart (terminal st)
         ( "+++ OK, passed "
        ++ show (numSuccessTests st)
        ++ " tests"
         )
      else
       putPart (terminal st)
         ( bold ("*** Failed!")
        ++ " Passed "
        ++ show (numSuccessTests st)
        ++ " tests (expected failure)"
         )
     success st
     theOutput <- terminalOutput (terminal st)
     if expectedFailure st then
       return Success{ labels = summary st,
                       numTests = numSuccessTests st,
                       output = theOutput }
      else
       return NoExpectedFailure{ labels = summary st,
                                 numTests = numSuccessTests st,
                                 output = theOutput }

giveUp :: State -> (QCGen -> Int -> Prop) -> IO Result
giveUp st _f =
  do -- CALLBACK gave_up?
     putPart (terminal st)
       ( bold ("*** Gave up!")
      ++ " Passed only "
      ++ show (numSuccessTests st)
      ++ " tests"
       )
     success st
     theOutput <- terminalOutput (terminal st)
     return GaveUp{ numTests = numSuccessTests st
                  , labels   = summary st
                  , output   = theOutput
                  }

logFailure :: P.Result -> State -> IO ()

failTestrun :: P.Result -> State -> IO Result


type ResultHandler = Args -> Hooks -> P.Result -> State -> (QCGen -> Int -> Prop) -> IO Result

handleSuccessResult :: ResultHandler
handleSuccessResult args hooks MkResult{stamp = stamp, expect = expect} st f
    | MkState{abort=True,finalResult=Just res'} <- st = handleResult args hooks res' st{finalResult=Nothing}
    | abort st   = doneTesting st' f
    | otherwise  = test args hooks st' f -- continue
    where
      st' = st{ numSuccessTests           = numSuccessTests st + 1
              , numRecentlyDiscardedTests = 0
              , randomSeed                = nextSeed st
              , collected                 = stamp : (collected st)
              , expectedFailure           = expect
              }
      
handleDiscardedResult :: ResultHandler
handleDiscardedResult args hooks MkResult{expect = expect} st f
    | abort st  = giveUp st' f
    | otherwise = test args hooks st' f --continue
    where
      st' = st{ numDiscardedTests         = numDiscardedTests st + 1
              , numRecentlyDiscardedTests = numRecentlyDiscardedTests st + 1
              , randomSeed                = nextSeed st
              , expectedFailure           = expect
              } f

handleFailedResult :: ResultHandler
handleFailedResult args hooks MkResult{stamp = stamp, expect = expect} st f
    | (keepGoing st) && (not $ abort st) = do putPart (terminal st) (bold "*** Failure detected! ... keep going! ")
                                           res <- failureResult
                                           test args hooks (nextState res) f
    | MkState{abort=True,finalResult=Just res'} <- st = handleResult args hooks res' st{finalResult=Nothing}
    | expect = do putPart (terminal st) (bold "*** Failed! ")
                  failureResult
    | (not expect) = do putPart (terminal st) "+++ OK, failed as expected. "
                      failureResult
    where 
      failureResult  
          | not expected = do (numShrinks, totFailed, lastFailed) <- foundFailure hooks st res ts
                            theOutput <- terminalOutput (terminal st)
                            return Failure{ usedSeed = randomSeed st -- correct! (this will be split first)
                                          , usedSize       = size
                                          , numTests       = numSuccessTests st+1
                                          , numShrinks     = numShrinks
                                          , numShrinkTries = totFailed
                                          , numShrinkFinal = lastFailed
                                          , output         = theOutput
                                          , reason         = P.reason res
                                          , theException   = P.theException res
                                          , labels         = summary st
                                          }
          | otherwise = do (numShrinks, totFailed, lastFailed) <- foundFailure hooks st res ts
                           theOutput <- terminalOutput (terminal st)
                           return Success{ labels = summary st,
                                           numTests = numSuccessTests st+1,
                                           output = theOutput }
      nextState res = st{ numSuccessTests           = numSuccessTests st + 1
                        , numRecentlyDiscardedTests = 0
                        , randomSeed                = nextSeed st
                        , collected                 = stamp : (collected st)
                        , expectedFailure           = expect
                        , finalResult               = maybe (Just res) Just (finalResult st) 
                        }

handleResult :: ResultHandler
handleResult args hooks res@(MkResult{ok = Just True}) = handleSuccessResult args hooks res
handleResult args hooks res@(MkResult{ok = Nothing}) = handleDiscardedResult args hooks res
handleResult args hooks res@(MkResult{ok = Just False}) = handleFailedResult args hooks res

runATest :: Args -> Hooks -> State -> (QCGen -> Int -> Prop) -> IO Result
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
     let size = computeSize st (numSuccessTests st) (numRecentlyDiscardedTests st)
     callbackPreTest hooks st
     MkRose res ts <- protectRose (reduceRose (unProp (f rnd1 size)))
     callbackPostTest hooks st res
     handleResult args hooks res st f

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
