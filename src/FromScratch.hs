{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FromScratch where

import Control.Monad
import Data.Tree
import qualified System.Random as R

import Util

{-------------------------------------------------------------------------------
  Manual shrinking
-------------------------------------------------------------------------------}

newtype Gen a = Gen { runGen :: R.StdGen -> a }

instance Functor Gen where
  fmap = liftM

instance Applicative Gen where
  pure  = return
  (<*>) = ap

instance Monad Gen where
  return a = Gen $ \_prng -> a
  f >>= g  = Gen $ \ prng ->
               let (prng1, prng2) = R.split prng
               in runGen (g (runGen f prng1)) prng2

-- | Bundle generator and manually defined shrinker
--
-- NOTE: invariant (a appears in negative and positive position)
data ManualGen a = MG {
      gen    :: Gen a
    , shrink :: a -> [a]
    }

{-------------------------------------------------------------------------------
  Test-driver for manual shrinking
-------------------------------------------------------------------------------}

type Seed = Int

mCheck :: Show a => Maybe Seed -> ManualGen a -> (a -> Bool) -> IO ()
mCheck mSeed gen p = do
    seed <- case mSeed of
              Just seed -> return seed
              Nothing   -> R.randomIO
    putStrLn $ "Using seed " ++ show seed
    mCheckWith seed gen p

mCheckWith :: Show a => Seed -> ManualGen a -> (a -> Bool) -> IO ()
mCheckWith seed gen p = do
    case mMinimalCounterExample seed gen p of
      Nothing -> putStrLn $ "OK"
      Just a  -> putStrLn $ "Counterexample: " ++ show a

mMinimalCounterExample :: forall a.
                          Seed -> ManualGen a -> (a -> Bool) -> Maybe a
mMinimalCounterExample seed MG{..} p = go (R.mkStdGen seed) numTests
  where
    numTests = 100

    go :: R.StdGen -> Int -> Maybe a
    go _    0 = Nothing
    go prng n = if not (p a)
                  then Just $ applyManualShrinker shrink p a
                  else go prng2 (n - 1)
      where
        (prng1, prng2) = R.split prng
        a = runGen gen prng1

-- | Apply manually defined shrinker to known-to-be-failing input
applyManualShrinker :: (a -> [a]) -> (a -> Bool) -> a -> a
applyManualShrinker f p a =
     case failed of
       []   -> a
       a':_ -> applyManualShrinker f p a'
  where
    shrunk = f a
    failed = filter (not . p) shrunk

{-------------------------------------------------------------------------------
  Examples of generators with manually defined shrinkers
-------------------------------------------------------------------------------}

-- Shrink maybe not optimal; orthogonal concern
mGenR :: (R.Random a, Enum a) => (a, a) -> ManualGen a
mGenR (lo, hi) = MG {
      gen    = Gen $ fst . R.randomR (lo, hi)
    , shrink = \a -> [a' | a' <- [lo .. pred a]]
    }

mGenPair :: ManualGen a -> ManualGen b -> ManualGen (a, b)
mGenPair genA genB = MG {
      gen    = (,) <$> gen genA <*> gen genB
    , shrink = \(a, b) -> concat [
                   [ (a', b ) | a' <- shrink genA a ]
                 , [ (a , b') | b' <- shrink genB b ]
                 , [ (a', b') | a' <- shrink genA a
                              , b' <- shrink genB b ]
                 ]
    }

{-------------------------------------------------------------------------------
  Example with manually defined shrinkers
-------------------------------------------------------------------------------}

mExample1 :: IO ()
mExample1 = mCheck Nothing (mGenPair mExampleInt mExampleInt) (uncurry (<))

mExample2 :: IO ()
mExample2 = mCheck Nothing (mGenPair mExampleInt mExampleInt) (uncurry (>))

-- Motivates the "parallel" shrinking case in mGenPair
mExample3 :: IO ()
mExample3  = mCheck Nothing (mGenPair mExampleInt mExampleInt) (uncurry (/=))

mExampleInt :: ManualGen Int
mExampleInt = mGenR (0, 10)

{-------------------------------------------------------------------------------
  Integrated shrinking
-------------------------------------------------------------------------------}

newtype IntGen a = IG { genTree :: R.StdGen -> Tree a }

runIntGen :: Seed -> IntGen a -> Tree a
runIntGen seed IG{..} = genTree (R.mkStdGen seed)

instance Functor IntGen where
  fmap = liftM

instance Applicative IntGen where
  pure  = return
  (<*>) = ap

instance Monad IntGen where
  return a = IG $ \_prng -> Node a []
  f >>= g  = IG $ \ prng ->
               let (prng1, prng2) = R.split prng
               in bindTree (genTree f prng1) (\a -> genTree (g a) prng2)
    where
      -- 'Tree' has a 'Monad' instance but it flips the order of subtrees
      -- We define our own for closer compatibility with hedgehog
      bindTree :: Tree x -> (x -> Tree y) -> Tree y
      bindTree (Node x ts) h =
        let Node y ts' = h x
        in Node y $ map (flip bindTree h) ts ++ ts'

intGen :: ManualGen a -> IntGen a
intGen MG{..} = IG $ unfoldTree (\a -> (a, shrink a)) . runGen gen

showTree :: Show a => Seed -> IntGen a -> IO ()
showTree seed gen = putStrLn $ drawTree $ show <$> runIntGen seed gen

{-------------------------------------------------------------------------------
  Looking for seeds (only useful for the blog post)
-------------------------------------------------------------------------------}

findTreeWithRoot :: forall a. (a -> Bool) -> IntGen a -> Seed
findTreeWithRoot p = findSeedSuchThat p'
  where
    p' :: Tree a -> Bool
    p' (Node a _) = p a

findSeedSuchThat :: (Tree a -> Bool) -> IntGen a -> Seed
findSeedSuchThat p gen = head $ filter p' ([0 ..])
  where
    p' :: Seed -> Bool
    p' seed = p (runIntGen seed gen)

{-------------------------------------------------------------------------------
  Test driver for integrated shrinking
-------------------------------------------------------------------------------}

iCheck :: Show a => Maybe Seed -> IntGen a -> (a -> Bool) -> IO ()
iCheck mSeed gen p = do
    seed <- case mSeed of
              Just seed -> return seed
              Nothing   -> R.randomIO
    putStrLn $ "Using seed " ++ show seed
    iCheckWith seed gen p

iCheckWith :: Show a => Seed -> IntGen a -> (a -> Bool) -> IO ()
iCheckWith seed gen p =
    case iMinimalCounterExample seed gen p of
      Nothing -> putStrLn $ "OK"
      Just a  -> putStrLn $ "Counterexample: " ++ show a

iMinimalCounterExample :: forall a. Seed -> IntGen a -> (a -> Bool) -> Maybe a
iMinimalCounterExample seed IG{..} p = go (R.mkStdGen seed) numTests
  where
    numTests = 100

    go :: R.StdGen -> Int -> Maybe a
    go _    0 = Nothing
    go prng n = if not (p (rootLabel tree))
                  then Just $ applyIntegratedShrinker p tree
                  else go prng2 (n - 1)
      where
        (prng1, prng2) = R.split prng
        tree = genTree prng1

-- | Apply integrated shrinking to known-to-have-failed test result
applyIntegratedShrinker :: (a -> Bool) -> Tree a -> a
applyIntegratedShrinker p tree =
    case failed of
      []      -> rootLabel tree
      tree':_ -> applyIntegratedShrinker p tree'
  where
    failed = filter (not . p . rootLabel) (subForest tree)

{-------------------------------------------------------------------------------
  Examples of generators with integrated shrinking
-------------------------------------------------------------------------------}

-- try seed 11
exampleIntGenInt :: Seed -> IO ()
exampleIntGenInt seed = showTree seed $ iExampleInt

-- try seed 6
exampleIntGenPair :: Seed -> IO ()
exampleIntGenPair seed = showTree seed $ twice iExampleInt

iExample1 :: IO ()
iExample1 = iCheck Nothing (twice iExampleInt) (uncurry (<))

iExample2 :: IO ()
iExample2 = iCheck Nothing (twice iExampleInt) (uncurry (>))

iExample3 :: IO ()
iExample3  = iCheck Nothing (twice iExampleInt) (uncurry (/=))

iExampleInt :: IntGen Int
iExampleInt = intGen mExampleInt
