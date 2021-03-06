{-# LANGUAGE ScopedTypeVariables #-}

module PrefixSum where

import Data.List (intercalate)
import Language.OpenCL.Host.Constants
import Language.OpenCL.Host.Core
import System.Environment (getArgs)
import System.Exit (exitFailure)

main = do ps <- getPlatforms
          args <- getArgs
          if null args && not (length ps == 1)
             then do putStrLn "You must specify a platform index; your platforms are:"
                     platformNames <- mapM (flip getPlatformInfo PlatformName) ps
                     putStrLn (intercalate ", " platformNames)
                     exitFailure
             else return ()
          let p | length ps == 1 = head ps
                | otherwise      = ps !! read (args !! 0)
          [gpu] <- getDevices p (bitSet [GPU])
          c <- createContext (pushContextProperty ContextPlatform p noProperties) [gpu]
          p <- createProgramWithSource c . (:[]) =<< readFile "prefixSum_kernels.cl"
          buildProgram p [gpu] ""
          square <- createKernel p "square"
          psStep <- createKernel p "prefixSumStep"
          (inBuf :: Buffer Int) <- createBuffer c (bitSet []) 5
          (outBuf :: Buffer Int) <- createBuffer c (bitSet []) 5
          setKernelArg square 0 inBuf
          setKernelArg psStep 1 inBuf
          setKernelArg psStep 2 outBuf
          q <- createCommandQueue c gpu (bitSet [])
          enqueueNonBlockingWriteBuffer q inBuf 0 [1,2,3,4,5] []
          enqueueNDRangeKernel q square [0] [5] [1] []
          flip mapM_ [0..3::Int] $ \ iteration ->
              do setKernelArg psStep 0 iteration
                 enqueueNDRangeKernel q psStep [0] [5] [1] []
                 enqueueCopyBuffer q outBuf inBuf 0 0 5 []
          ev <- enqueueNonBlockingReadBuffer q inBuf 0 5 print []
          putStrLn "waiting"
          waitForEvents [ev]
