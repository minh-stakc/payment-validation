module Main (main) where

import Test.Hspec
import qualified ValidationSpec

main :: IO ()
main = hspec ValidationSpec.spec
