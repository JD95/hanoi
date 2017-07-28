module Main where

import Data.List
import Control.Arrow

newtype Disk = Disk Int deriving (Show, Eq)

initPeg :: Int -> [Int]
initPeg = flip replicate 1

applyToSpot l n f = replicate (n - 1) id ++ [f] ++ replicate (l - 1) id

moveDisk nPegs n = applyToSpot nPegs n $ \x -> ((x + (n `mod` 2)) `mod` nPegs) + 1

moves 1 = [1]
moves n = let side = moves (n - 1) in side ++ [n] ++ side

record :: (a -> a) -> (([a], a) -> ([a], a)) -> ([a], a) -> ([a], a)
record g f = (uncurry (flip (:)) &&& snd) . second g . f                    

solution = foldr record id $ fmap (zipWith ($) . moveDisk 3) (moves 3)

showPegs :: [Int] -> String
showPegs ps = unlines . fmap unwords . transpose . fmap (pad 3 "   ") $ fill blank
   where disks = fmap ((:) . reverse . pad 3 ' ' . flip replicate '*') [1.. (length ps)]
         fill = foldr (.) id $ fmap (zipWith ($)) (zipWith ($) (fmap (applyToSpot (length ps)) ps) disks)
         blank = replicate (length ps) []
         pad n p xs = if length xs < n
                         then replicate (n - length xs) p ++ xs
                         else xs

main :: IO ()
main = mapM_ putStrLn
     . fmap showPegs
     . reverse . fst
     . solution
     $ ([], initPeg 3)
