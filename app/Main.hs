module Main where

import Control.Arrow    (Arrow ((&&&)))
import Data.List        (genericLength, group, sort)
import System.Directory (listDirectory)
import System.FilePath  (takeExtension)
import Text.Printf      (printf)

main :: IO ()
main = printExts . pctElems . filter (/="") . fmap takeExtension =<< listDirectory "."
  where
    printExts :: [(Double, String)] -> IO ()
    printExts = mapM_ . uncurry . printf $ "%6.02f %%  %s\n"

-- | Get percentage of each element in a list
pctElems :: (Ord a, Fractional b) => [a] -> [(b, a)]
pctElems xs = (calcPct . genericLength &&& head) <$> group (sort xs)
  where
    len     = genericLength xs
    calcPct = (*100) . (/len)
