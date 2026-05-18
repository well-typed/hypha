module Main (main) where

import qualified Hypha.Prelude as Hypha

main :: IO ()
main = putStrLn ("hypha " <> Hypha.version)
