{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances, TypeFamilies, FunctionalDependencies, Rank2Types #-}
module Main where

import Test (residual_0)
import Test.QuickCheck
import Test.QuickCheck.Gen
--import Data.Tuple.Curry
import Control.DeepSeq
import System.Random
import System.TimeIt
import Data.List
import Text.Printf
import System.IO
import System.Environment
import Data.Serialize
import qualified Data.ByteString as B


class (Arbitrary a, Serialize a, NFData a, Eq a, Show a) => ImplementsStuff a
instance (Arbitrary a, Serialize a, NFData a, Eq a, Show a) => ImplementsStuff a

class Benchmarkable f where
    myuncurry :: f -> (forall a b . (ImplementsStuff a, ImplementsStuff b) => (a -> b) -> r) -> r

instance Benchmarkable ((a,b) -> c) => Benchmarkable (a -> b -> c) where
    myuncurry f g = myuncurry (uncurry f) g

instance (ImplementsStuff a, ImplementsStuff b) => Benchmarkable (a -> b) where
    myuncurry f g = g f

{-# NOINLINE evaluateInIO #-}
evaluateInIO x = x `deepseq` return ()

timePrefixN :: NFData a => Double -> [a] -> IO Int
timePrefixN tm lst = go tm 1 lst 0
    where 
      go tm k lst total = do
        --printf "go %.2f %d %d\n" tm k total
        let (pref, rest) = splitAt k lst
        (d,_) <- timeItT $ evaluateInIO pref
        --printf "d = %.2f\n" d
        if d >= tm || null rest then
            return $ total + k
        else do
            let newk | d < 0.00001 = k * 2
                     | otherwise  = max 1 (floor ((fromIntegral k) * 0.01 / d))
            go (tm - d) (min 1000000 newk) rest (total + k)

timePrefix :: NFData a => Double -> [a] -> IO [a]
timePrefix tm lst = do
    k <- timePrefixN tm lst
    return $ take k lst

generateTestData :: (Arbitrary a, NFData a) => [Double] -> [Int] -> [Int] -> IO [[a]]
generateTestData tms counts sizes = do
    let gen = sequence (map (\(c,s) -> vectorOf c (resize s arbitrary)) (zip counts sizes))
    stdgen <- newStdGen
    let dat = unGen gen stdgen 1
    sequence $ map (uncurry timePrefix) (zip tms dat)


streamOf :: Gen a -> Gen [a]
streamOf gen = sequence (repeat gen)



mainGenerate f = do
    hPutStrLn stderr "generating..."
    let sizes = [1,2..9] ++ [10,20..100]
    let counts = map (\s -> min 10000 (2 ^ min 30 s)) sizes
    let times = repeat 0.5 --map (\s -> min (fromIntegral s * 0.1) 1.0) sizes
    uncycled <- {-fmap (map nub) $-} generateTestData times counts sizes
    hPrint stderr $ map length uncycled
    hPrint stderr $ take 5 uncycled
    
    let dat = map cycle $ filter (not . null) uncycled
    let ress = map (map f) dat
    let retained = map (take 3) ress
    -- We spine-evaluate retained to get rid of thunks holding ress which prevent it from being gced
    evaluateInIO (map (map (const ())) retained)
    let exectimes = map (\s -> max 1.0 $ min 0.5 $ (0.5 + 0.1 * fromIntegral s)) sizes
    lens <- sequence $ map (uncurry timePrefixN) (zip exectimes ress)
    hPrint stderr lens

    hPrint stderr $ take 5 retained

    let outdata = zip (zip lens sizes) $ map (uncurry take) (zip lens uncycled)
    B.putStr (encode (outdata, retained))
    

mainRun f = do
    hPutStrLn stderr "Reading data..."
    bs <- B.getContents
    let (Right (predat, correct_ress)) = decode bs
    evaluateInIO predat
    hPutStrLn stderr $ "Sizes:   " ++ show (map (snd . fst) predat :: [Int])
    hPutStrLn stderr $ "Lengths: " ++ show (map (fst . fst) predat :: [Int])
    let ress = [map f (take len (cycle xs)) | ((len,_),xs) <- predat]
    let retained = map (uncurry take) (zip (map length correct_ress) ress)
    evaluateInIO (map (map (const ())) retained)
    (total,times) <- timeItT $ sequence $ map (fmap fst . timeItT . evaluateInIO) ress
    printf "#correct %s\n" $ show $ and [r == cr | (rl,crl) <- retained `zip` correct_ress, (r,cr) <- rl `zip` crl]
    printf "#times %s\n" (unwords $ map (printf "%.2f") times)
    printf "#time %.2f\n" total
    

main = do
    as <- getArgs
    case as of
        "gen":_ -> myuncurry residual_0 mainGenerate
        "run":_ -> myuncurry residual_0 mainRun
 

