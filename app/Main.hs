module Main where

import Control.Arrow    (Arrow (first, (&&&)))
import Data.Functor     ((<&>))
import Data.List        (genericLength, group, sort)
import System.Directory (listDirectory)
import System.FilePath  (takeExtension)
import Text.Printf      (printf)

main :: IO ()
main = printExts . getPctExts =<< listDirectory "."
  where
    printExts :: [(Double, String)] -> IO ()
    printExts = mapM_ . uncurry . printf $ "%.2f %%  %s\n"

-- | Get percentage of file extensions
getPctExts :: [FilePath] -> [(Double, String)]
getPctExts = fmap takeExtension
  <&> filter (/="")
  <&> group . sort
  <&> fmap (genericLength &&& head)
  <&> id &&& genericLength
  <&> \(ys, len) -> first ((*100) . (/len)) <$> ys
