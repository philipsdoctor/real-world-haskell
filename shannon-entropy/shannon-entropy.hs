module Main (main) where

import Data.Map.Lazy as Map
import Text.Printf

main :: IO ()
main = interact displayEntropy
    where displayEntropy input = printf "Shannon Entropy is %.4f\n" (shannonEntropy (wordHistogram input))

wordHistogram :: String -> Map.Map String Int
wordHistogram string = Prelude.foldl increment emptyMap (words string)
    where
        emptyMap = fromList []
        increment :: Map.Map String Int -> String -> Map String Int
        increment theMap theString = alter maybeAddOne theString theMap
        maybeAddOne :: Maybe Int -> Maybe Int
        maybeAddOne = Just.(maybe 1 (1 +))

shannonEntropy :: Map String Int -> Float
shannonEntropy theMap = -1 * (Map.foldl someFunc 0.0 theMap)
    where
        totes :: Int
        totes = Map.foldl (+) 0 theMap
        someFunc :: Float -> Int -> Float
        someFunc a b = a + freq * (log freq)
            where
                freq :: Float
                freq = fromIntegral b / fromIntegral totes
