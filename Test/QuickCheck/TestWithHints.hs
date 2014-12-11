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
import Trace.Hpc.Mix
import Trace.Hpc.Util
import Data.IORef
import Data.List
import Data.Ord
import Control.Applicative
import qualified Data.ByteString.Char8 as Str

data Hint = Hint {hintFile :: String
                 , hintContext :: [(Int,String)]
                 , hintScore   :: Integer }
          deriving Show


data HintArgs = HintArgs { mixPath :: [String]      -- | The search path where the mix files can be found
                         , moduleNames :: [String]  -- | The module names that should be considered for the hints (maybe you don't want to consider the test source itself...)                       
                         }

quickCheckWithHints :: Testable prop => HintArgs -> prop -> IO ()
quickCheckWithHints (HintArgs mixPath moduleNames) p = do
                        (res, Tix modules) <- quickCheckWithTix moduleNames stdArgs p
                        hintList <- concat <$> (mapM (analyzeModule mixPath)  modules)
                        let topHints = take 20 $ sortBy (comparing (Down . hintScore)) hintList
                        putStrLn ""
                        putStrLn "-----------------------"
                        putStrLn "         Hints         "
                        putStrLn "-----------------------"
                        mapM_ printHint topHints
                        return ()

printHint :: Hint -> IO ()
printHint (Hint file context score) = do
  putStrLn $ " ---> In " ++ (show file) ++ "(Score: " ++ (show score) ++ "):"
  mapM_ (\ (lineNo, str) -> putStrLn $ "   " ++ (show lineNo) ++ ". " ++ str) context

analyzeModule :: [String] -> TixModule -> IO [Hint]
analyzeModule mixPath m@(TixModule name hash len ticks) = do
  (Mix file time hash tabStop entries) <- readMix mixPath (Right m)
  mapM (createHint file) (zip entries ticks)

createHint :: String -> (MixEntry, Integer) -> IO Hint
createHint file ((pos,lbl), score) = do
  contents <- Str.readFile file
  let codeLines = Str.lines contents
      (lineStart, colStart, lineEnd, colEnd) = fromHpcPos pos
      from = max (lineStart-2) 0
      to = min (lineEnd+1) (length codeLines)
      context = sublist from to (zip [1..] codeLines)
      formatLine (lineIndex, str) 
          | (lineIndex == lineStart) && (lineIndex == lineEnd) 
              = let (left,right') = Str.splitAt (colStart-1) str
                    (middle,right) = Str.splitAt (colEnd - colStart + 1) right'
                in  (lineIndex, (Str.unpack left) ++ "\x1b[32m" ++ (Str.unpack middle) ++ "\x1b[0m" ++ (Str.unpack right))
          | lineIndex == lineStart 
              = let (left,right) = Str.splitAt (colStart-1) str
                in  (lineIndex, (Str.unpack left) ++ "\x1b[32m" ++ (Str.unpack right))
          | lineIndex == lineEnd 
              = let (left,right) = Str.splitAt (colEnd-1) str
                in  (lineIndex, (Str.unpack left) ++ "\x1b[0m" ++ (Str.unpack right))
          | otherwise = (lineIndex, Str.unpack str)
  return $ Hint file (map formatLine context) score
    


sublist from to lst = take (to-from) . drop from $ lst

quickCheckWithTix :: Testable prop => [String] -> Args -> prop -> IO (Result, Tix)
quickCheckWithTix moduleNames args p = do
  clearTix
  emptyTix <- filterByModules moduleNames <$> examineTix
  successTixRef <- newIORef emptyTix
  failureTixRef <- newIORef emptyTix
  let args' = args { keepGoing = True }
      hooks = Hooks { preTestHook = preTest
                    , postTestHook = postTest }
      preTest :: State -> IO ()
      preTest st = clearTix
      postTest :: State -> P.Result -> IO ()
      postTest st res = case P.ok res of
                          Just True -> updateTixRef moduleNames successTixRef
                          Just False -> updateTixRef moduleNames failureTixRef                        
  res <- quickCheckWithHooks args' hooks p
  successTix <- readIORef successTixRef
  failureTix <- readIORef failureTixRef
  let scoreTix = applyTix (score 2) successTix failureTix
  return (res, scoreTix)
        
-- Tix [TickModule "name" hash length-of-list [ticks]]

updateTixRef :: [String] -> IORef Tix -> IO ()
updateTixRef moduleNames tixref = do
  currentTix <- readIORef tixref
  newTix <- filterByModules moduleNames <$> examineTix
  writeIORef tixref (addTix currentTix newTix)


addTix :: Tix -> Tix -> Tix
addTix = applyTix (+)

filterByModules :: [String] -> Tix -> Tix
filterByModules moduleNames = filterTix ( \ (TixModule name _ _ _) -> name `elem` moduleNames)

filterTix :: (TixModule -> Bool) -> Tix -> Tix
filterTix p (Tix modules) = Tix $ filter p modules

applyTix :: (Integer -> Integer -> Integer) -> Tix -> Tix -> Tix
applyTix f (Tix modules) (Tix modules') = Tix $ zipWith applyTix' modules modules'
    where
      applyTix' (TixModule name hash len ticks) (TixModule name' _ len' ticks')
          | name /= name' || len /= len' = error "encountered incompatible Tix definitions!"
          | otherwise = TixModule name hash len ( zipWith f ticks ticks' )


score :: Integer -> Integer -> Integer -> Integer
score minTests success failure
    | success + failure < minTests = 0
    | otherwise = quot (1000 * failure) (success + failure)
