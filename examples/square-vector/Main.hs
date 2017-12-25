{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Main where

import Language.OpenCL.Host
import Language.OpenCL.Host.Core
import Language.OpenCL.Host.Constants

main :: IO ()
main = do
    putStrLn "Hello World"

    -- Find an OpenCL Platform to use
    plats <- getPlatforms
    let
        plat =
            case plats of
                (p:_) -> p
                []    -> error "No OpenCL platforms found."

    -- Write out information about the platform
    putStrLn "=== OpenCL Platform Information ==="
    platProfile <- plat ? PlatformProfile
    platVersion <- plat ? PlatformVersion
    platName    <- plat ? PlatformName
    platVendor  <- plat ? PlatformVendor
    platExten   <- plat ? PlatformExtensions
    putStrLn $ "Profile:    " ++ show platProfile
    putStrLn $ "Version:    " ++ show platVersion
    putStrLn $ "Name:       " ++ show platName
    putStrLn $ "Vendor:     " ++ show platVendor
    putStrLn $ "Extensions: " ++ show platExten
    putStrLn ""

    -- Find a CPU Device to use (just using the first one)
    devs <- getDevices plat (bitSet [CPU])
    let
        dev :: Device
        dev =
            case devs of
                (d:_) -> d
                []    -> error "No OpenCL CPU device found."
    -- Write out information about the device
    putStrLn "=== OpenCL Device Information ==="
    devType   <- dev ? DeviceType
    devVendor <- dev ? DeviceVendor
    devName   <- dev ? DeviceName
    putStrLn $ "Value:  " ++ show dev
    putStrLn $ "Type:   " ++ show devType
    putStrLn $ "Vendor: " ++ show devVendor
    putStrLn $ "Name:   " ++ show devName
    
    -- Create OpenCL Context
    let
        props = pushContextProperty ContextPlatform plat noProperties
    ctxt <- createContext props [dev]

    releaseContext ctxt
    
    putStrLn "DONE"
