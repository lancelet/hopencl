module Main where

import Language.OpenCL.Host
import Language.OpenCL.Host.FFI

main :: IO ()
main = do
    [p] <- platforms
    let vendors = undefined
    return ()
