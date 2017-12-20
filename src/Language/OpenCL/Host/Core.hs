{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Language.OpenCL.Host.Core where

import           Language.OpenCL.Host.Constants (BitSet, CLEventCallback,
                                                 CLImageFormat, CLInt, CLMem,
                                                 CLUInt, CommandQueue,
                                                 CommandQueueInfo,
                                                 CommandQueueProperty, Const,
                                                 Context, ContextInfo,
                                                 ContextProperties, Device,
                                                 DeviceInfo, DeviceType, Event,
                                                 EventCallback, EventInfo,
                                                 EventStatus, ImageInfo, Kernel,
                                                 KernelInfo,
                                                 KernelWorkGroupInfo, MemFlag,
                                                 MemInfo, MemObjectType,
                                                 Platform, PlatformInfo,
                                                 ProfilingInfo, Program,
                                                 ProgramBuildInfo, ProgramInfo,
                                                 SizeT)
import qualified Language.OpenCL.Host.Constants as Co
import qualified Language.OpenCL.Host.FFI       as CL

import           Control.Exception              (Exception, catch, throw)
import           Data.Array.Storable            (StorableArray, getBounds,
                                                 newArray_, withStorableArray)
import           Data.Typeable                  (Typeable)
import           Foreign                        (Bits, Ptr, Storable, alignment,
                                                 alloca, allocaArray, castPtr,
                                                 free, malloc, mallocArray,
                                                 newArray, nullFunPtr, nullPtr,
                                                 peek, peekArray, peekElemOff,
                                                 poke, pokeArray, pokeElemOff,
                                                 sizeOf, withArray)
import           Foreign.C                      (CChar, newCString, peekCString,
                                                 withCString)

import           Prelude                        (Bool (False, True), Bounded,
                                                 Char, Enum, Eq, IO, Int,
                                                 Integral,
                                                 Maybe (Just, Nothing), Num,
                                                 Ord, Show, String, concatMap,
                                                 const, div, error, filter,
                                                 fmap, fromIntegral, length,
                                                 lookup, map, mapM_, maxBound,
                                                 minBound, null, otherwise,
                                                 return, show, snd, take,
                                                 undefined, zip, ($), (*), (+),
                                                 (++), (-), (.), (/=), (<),
                                                 (==), (>>=))

data CLError
    = DeviceNotFound | DeviceNotAvailable | CompilerNotAvailable
    | MemObjectAllocationFailure | OutOfResources | OutOfHostMemory
    | ProfilingInfoNotAvailable | MemCopyOverlap | ImageFormatMismatch
    | ImageFormatNotSupported | BuildProgramFailure | MapFailure
    | MisalignedSubBufferOffset | ExecStatusErrorForEventsInWaitList
    | InvalidValue | InvalidDeviceType | InvalidPlatform | InvalidDevice
    | InvalidContext | InvalidQueueProperties | InvalidCommandQueue
    | InvalidHostPtr | InvalidMemObject | InvalidImageFormatDescriptor
    | InvalidImageSize | InvalidSampler | InvalidBinary | InvalidBuildOptions
    | InvalidProgram | InvalidProgramExecutable | InvalidKernelName
    | InvalidKernelDefinition | InvalidKernel | InvalidArgIndex
    | InvalidArgValue | InvalidArgSize | InvalidKernelArgs
    | InvalidWorkDimension | InvalidWorkGroupSize | InvalidWorkItemSize
    | InvalidGlobalOffset | InvalidEventWaitList | InvalidEvent
    | InvalidOperation | InvalidGLObject | InvalidBufferSize | InvalidMipLevel
    | InvalidGlobalWorkSize
    deriving (Bounded, Enum, Eq, Ord, Typeable)

instance Co.Const CLError CLInt where
    value DeviceNotFound                     = -1
    value DeviceNotAvailable                 = -2
    value CompilerNotAvailable               = -3
    value MemObjectAllocationFailure         = -4
    value OutOfResources                     = -5
    value OutOfHostMemory                    = -6
    value ProfilingInfoNotAvailable          = -7
    value MemCopyOverlap                     = -8
    value ImageFormatMismatch                = -9
    value ImageFormatNotSupported            = -10
    value BuildProgramFailure                = -11
    value MapFailure                         = -12
    value MisalignedSubBufferOffset          = -13
    value ExecStatusErrorForEventsInWaitList = -14
    value InvalidValue                       = -30
    value InvalidDeviceType                  = -31
    value InvalidPlatform                    = -32
    value InvalidDevice                      = -33
    value InvalidContext                     = -34
    value InvalidQueueProperties             = -35
    value InvalidCommandQueue                = -36
    value InvalidHostPtr                     = -37
    value InvalidMemObject                   = -38
    value InvalidImageFormatDescriptor       = -39
    value InvalidImageSize                   = -40
    value InvalidSampler                     = -41
    value InvalidBinary                      = -42
    value InvalidBuildOptions                = -43
    value InvalidProgram                     = -44
    value InvalidProgramExecutable           = -45
    value InvalidKernelName                  = -46
    value InvalidKernelDefinition            = -47
    value InvalidKernel                      = -48
    value InvalidArgIndex                    = -49
    value InvalidArgValue                    = -50
    value InvalidArgSize                     = -51
    value InvalidKernelArgs                  = -52
    value InvalidWorkDimension               = -53
    value InvalidWorkGroupSize               = -54
    value InvalidWorkItemSize                = -55
    value InvalidGlobalOffset                = -56
    value InvalidEventWaitList               = -57
    value InvalidEvent                       = -58
    value InvalidOperation                   = -59
    value InvalidGLObject                    = -60
    value InvalidBufferSize                  = -61
    value InvalidMipLevel                    = -62
    value InvalidGlobalWorkSize              = -63

instance Show CLError where
    show DeviceNotFound                     = "Device not found"
    show DeviceNotAvailable                 = "Device not available"
    show CompilerNotAvailable               = "Compiler not available"
    show MemObjectAllocationFailure         = "Memory object allocation failure"
    show OutOfResources                     = "Out of resources"
    show OutOfHostMemory                    = "Out of host memory"
    show MemCopyOverlap                     = "Memory copy overlap"
    show ImageFormatMismatch                = "Image format mismatch"
    show ImageFormatNotSupported            = "Image format not supported"
    show BuildProgramFailure                = "Program build failure"
    show MapFailure                         = "Map failure"
    show MisalignedSubBufferOffset          = "Misaligned subbuffer offset"
    show InvalidValue                       = "Invalid value"
    show InvalidDeviceType                  = "Invalid device type"
    show InvalidPlatform                    = "Invalid platform"
    show InvalidDevice                      = "Invalid device"
    show InvalidContext                     = "Invalid context"
    show InvalidQueueProperties             = "Invalid queue properties"
    show InvalidCommandQueue                = "Invalid command queue"
    show InvalidHostPtr                     = "Invalid host pointer"
    show InvalidMemObject                   = "Invalid memory object"
    show InvalidImageFormatDescriptor       = "Invalid image format descriptor"
    show InvalidImageSize                   = "Invalid image size"
    show InvalidSampler                     = "Invalid sampler"
    show InvalidBinary                      = "Invalid binary"
    show InvalidBuildOptions                = "Invalid build options"
    show InvalidProgram                     = "Invalid program"
    show InvalidProgramExecutable           = "Invalid program executable"
    show InvalidKernelName                  = "Invalid kernel name"
    show InvalidKernelDefinition            = "Invalid kernel definition"
    show InvalidKernel                      = "Invalid kernel"
    show InvalidArgIndex                    = "Invalid argument index"
    show InvalidArgValue                    = "Invalid argument value"
    show InvalidArgSize                     = "Invalid argument size"
    show InvalidKernelArgs                  = "Invalid kernel arguments"
    show InvalidWorkDimension               = "Invalid work dimension"
    show InvalidWorkGroupSize               = "Invalid work group size"
    show InvalidWorkItemSize                = "Invalid work item size"
    show InvalidGlobalOffset                = "Invalid global offset"
    show InvalidEventWaitList               = "Invalid event wait list"
    show InvalidEvent                       = "Invalid event"
    show InvalidOperation                   = "Invalid operation"
    show InvalidGLObject                    = "Invalid GL object"
    show InvalidBufferSize                  = "Invalid buffer size"
    show InvalidMipLevel                    = "Invalid mip level"
    show InvalidGlobalWorkSize              = "Invalid global work size"
    show ProfilingInfoNotAvailable =
        "Profiling information not available"
    show ExecStatusErrorForEventsInWaitList =
        "Execution status error for events in wait list"

data Error
    = CLError CLError [String]
    | UserError String [String]
    deriving (Eq, Typeable)

appendLocation :: String -> Error -> Error
appendLocation s (CLError err location)   = CLError err (location ++ [s])
appendLocation s (UserError err location) = UserError err (location ++ [s])

appendingLocation :: String -> IO t -> IO t
appendingLocation location comp = comp `catch` appender
  where
    appender :: Error -> IO t
    appender err = throw $ appendLocation location err

instance Show Error where
    show (CLError c location) =
        "OpenCL error: " ++ show c ++ concatMap ("\n    at " ++) location
    show (UserError s location) =
        "User error: " ++ s ++ concatMap ("\n    at " ++) location

instance Exception Error

errorFromErrorCode :: CLInt -> Error
errorFromErrorCode errc =
    case lookup errc [(Co.value c, c) | c <- [minBound..maxBound]] of
      Nothing  -> UserError ("Uninterpreted error code " ++ show errc) []
      Just err -> CLError err []

clFail :: CLInt -> String -> a
clFail errc location = throw (appendLocation location $ errorFromErrorCode errc)

userFail :: String -> String -> a
userFail s location = throw (UserError s [location])

handleCLError :: String -> IO CLInt -> IO ()
handleCLError location cl =
    do
        r <- cl
        if r /= 0 then clFail r location else return ()

handleCLCreationError :: String -> (Ptr CLInt -> IO t) -> IO t
handleCLCreationError location ctor =
    alloca $ \errPtr ->
        do
            v <- ctor errPtr
            r <- peek errPtr
            if r /= 0 then clFail r location else return v

handleCLEventDriven :: String
                    -> (CLUInt -> Ptr Event -> Ptr Event -> IO CLInt)
                    -> [Event]
                    -> IO Event
handleCLEventDriven location cl events =
    if null events
    then alloca $ \evPtr ->
            do
                handleCLError location $ cl 0 nullPtr evPtr
                peek evPtr
    else
        let
            nEvents = length events
        in
            withArray events
            $ \evArray ->
                  alloca
                  $ \evPtr ->
                        do
                            handleCLError location
                                $ cl (fromIntegral nEvents) evArray evPtr
                            peek evPtr

-- Generic getters

getStorable :: forall t. Storable t
            => (SizeT -> Ptr t -> Ptr SizeT -> IO CLInt)
            -> IO t
getStorable getter =
    alloca
    $ \ptr ->
          do
              handleCLError "getStorable"
                  $ getter (fromIntegral (sizeOf (undefined :: t))) ptr nullPtr
              peek ptr

getBitSet :: forall t u. (Const t u, Integral u, Bits u, Storable u)
          => (SizeT -> Ptr u -> Ptr SizeT -> IO CLInt)
          -> IO (BitSet t)
getBitSet getter =
    alloca
    $ \ptr ->
          do
              handleCLError "getBitSet"
                  $ getter (fromIntegral (sizeOf (undefined :: u))) ptr nullPtr
              Co.BitSet `fmap` peek ptr

getConstEnum :: forall t u. ( Const t u
                            , Bounded t
                            , Enum t
                            , Eq u
                            , Storable u
                            , Show u
                            )
             => (SizeT -> Ptr u -> Ptr SizeT -> IO CLInt)
             -> IO t
getConstEnum getter =
    alloca
    $ \ptr ->
          do
              handleCLError "getConstEnum"
                  $ getter (fromIntegral (sizeOf (undefined :: u))) ptr nullPtr
              v <- peek ptr
              case filter ((v ==) . snd)
                          [(c, Co.value c) | c <- [minBound .. maxBound]] of
                  ((c, _):_) -> return c
                  _          -> error $ "Unable to interpret " ++ show v

genericGetArray :: forall t u. Storable u
                => (Int -> Ptr u -> IO [t])
                -> (SizeT -> Ptr u -> Ptr SizeT -> IO CLInt)
                -> IO [t]
genericGetArray peeker getter =
    do
        sizeValue <- alloca
                     $ \sizePtr ->
                           do
                               handleCLError "genericGetArray"
                                   $ getter 0 nullPtr sizePtr
                               peek sizePtr
        let nElements = fromIntegral sizeValue `div` sizeOf (undefined :: u)
        allocaArray nElements
            $ \ptr ->
                  do
                      handleCLError "genericGetArray"
                          $ getter sizeValue ptr nullPtr
                      peeker nElements ptr

-- FIXME: this doesn't work.  Output of 'compile' example, for instance
getString :: (SizeT -> Ptr CChar -> Ptr SizeT -> IO CLInt) -> IO [Char]
getString = appendingLocation "getString" . genericGetArray (const peekCString)

getArray :: forall t. Storable t
         => (SizeT -> Ptr t -> Ptr SizeT -> IO CLInt)
         -> IO [t]
getArray = appendingLocation "getArray" . genericGetArray peekArray

getCountedArray :: forall t. Storable t
                => (CLUInt -> Ptr t -> Ptr CLUInt -> IO CLInt)
                -> IO [t]
getCountedArray getter =
    do count <- alloca $ \ countPtr ->
           do handleCLError "getCountedArray" $ getter 0 nullPtr countPtr
              peek countPtr
       let count' = fromIntegral count
       allocaArray count' $ \ ptr ->
           do handleCLError "getCountedArray" $ getter count ptr nullPtr
              peekArray count' ptr

--
-- Platform stuff
--

getPlatforms :: IO [Platform]
getPlatforms = appendingLocation "getPlatforms" $
               getCountedArray CL.clGetPlatformIDs

getPlatformInfo :: Platform -> PlatformInfo u -> IO u
getPlatformInfo platform plInf =
    case plInf of
        Co.PlatformProfile    -> get
        Co.PlatformVersion    -> get
        Co.PlatformName       -> get
        Co.PlatformVendor     -> get
        Co.PlatformExtensions -> get
  where
    get = appendingLocation "getPlatformInfo"
          $ getString (CL.clGetPlatformInfo platform (Co.value plInf))

--
-- Device stuff
--

getDevices :: Platform -> BitSet DeviceType -> IO [Device]
getDevices platform devTypes =
    appendingLocation "getDevices"
    $ getCountedArray (CL.clGetDeviceIDs platform (Co.valueFrom devTypes))

getDeviceInfo :: Device -> DeviceInfo t -> IO t
getDeviceInfo device devInfo =
    case devInfo of
      Co.DeviceType                     -> bitSetHelper
      Co.DeviceVendorID                 -> stoHelper
      Co.DeviceMaxComputeUnits          -> stoHelper
      Co.DeviceMaxWorkItemDimensions    -> stoHelper
      Co.DeviceMaxWorkItemSizes         -> arrayHelper

      Co.DeviceMaxWorkGroupSize         -> stoHelper
      Co.DevicePreferredVectorWidth _   -> stoHelper
      Co.DeviceNativeVectorWidth _      -> Co.unsupported
      Co.DeviceMaxClockFrequency        -> stoHelper
      Co.DeviceAddressBits              -> stoHelper

      Co.DeviceMaxMemAllocSize          -> stoHelper

      Co.DeviceImageSupport             -> stoHelper
      Co.DeviceMaxReadImageArgs         -> stoHelper
      Co.DeviceMaxWriteImageArgs        -> stoHelper
      Co.DeviceImage2DMaxWidth          -> stoHelper
      Co.DeviceImage2DMaxHeight         -> stoHelper
      Co.DeviceImage3DMaxWidth          -> stoHelper
      Co.DeviceImage3DMaxHeight         -> stoHelper
      Co.DeviceImage3DMaxDepth          -> stoHelper
      Co.DeviceMaxSamplers              -> stoHelper

      Co.DeviceMaxParameterSize         -> stoHelper

      Co.DeviceMemBaseAddrAlign         -> stoHelper
      Co.DeviceMinDataTypeAlignSize     -> stoHelper

      Co.DeviceSingleFPConfig           -> bitSetHelper

      Co.DeviceGlobalMemCacheType       -> constEnumHelper
      Co.DeviceGlobalMemCachelineSize   -> stoHelper
      Co.DeviceGlobalMemCacheSize       -> stoHelper
      Co.DeviceGlobalMemSize            -> stoHelper

      Co.DeviceMaxConstantBufferSize    -> stoHelper
      Co.DeviceMaxConstantArgs          -> stoHelper

      Co.DeviceLocalMemType             -> constEnumHelper
      Co.DeviceLocalMemSize             -> stoHelper
      Co.DeviceErrorCorrectionSupport   -> stoHelper

      Co.DeviceHostUnifiedMemory        -> stoHelper

      Co.DeviceProfilingTimerResolution -> stoHelper

      Co.DeviceEndianLittle             -> stoHelper

      Co.DeviceAvailable                -> stoHelper
      Co.DeviceCompilerAvailable        -> stoHelper

      Co.DeviceExecutionCapabilities    -> bitSetHelper

      Co.DeviceQueueProperties          -> bitSetHelper

      Co.DevicePlatform                 -> stoHelper

      Co.DeviceName                     -> stringHelper
      Co.DeviceVendor                   -> stringHelper
      Co.DriverVersion                  -> stringHelper
      Co.DeviceProfile                  -> stringHelper
      Co.DeviceVersion                  -> stringHelper
      Co.DeviceOpenCLCVersion           -> stringHelper
      Co.DeviceExtensions               -> stringHelper

  where

    getter :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
    getter = CL.clGetDeviceInfo device (Co.value devInfo)

    stoHelper :: forall t. Storable t => IO t
    stoHelper = getStorable getter

    bitSetHelper :: forall t u. (Const t u, Integral u, Bits u, Storable u)
                 => IO (BitSet t)
    bitSetHelper = getBitSet getter

    arrayHelper :: forall t. Storable t => IO [t]
    arrayHelper = getArray getter

    stringHelper :: IO String
    stringHelper = getString getter

    constEnumHelper :: forall t u. ( Enum t
                                   , Bounded t
                                   , Const t u
                                   , Eq u
                                   , Storable u
                                   , Show u
                                   )
                    => IO t
    constEnumHelper = getConstEnum getter

--
-- Contexts
--

createContext :: ContextProperties -> [Device] -> IO Context
createContext properties devices =
    Co.withContextProperties properties
    $ \contextProperties ->
          withArray devices
          $ \deviceArray ->
                handleCLCreationError "createContext"
                $ CL.clCreateContext contextProperties
                                     (fromIntegral (length devices))
                                     deviceArray
                                     nullFunPtr
                                     nullPtr

createContextFromType :: ContextProperties -> BitSet DeviceType -> IO Context
createContextFromType properties devTypes =
    Co.withContextProperties properties
    $ \contextProperties ->
          handleCLCreationError "createContextFromType"
          $ CL.clCreateContextFromType contextProperties
                                       (Co.valueFrom devTypes)
                                       nullFunPtr
                                       nullPtr

retainContext :: Context -> IO ()
retainContext = handleCLError "retainContext" . CL.clRetainContext

releaseContext :: Context -> IO ()
releaseContext = handleCLError "retainContext" . CL.clReleaseContext

getContextInfo :: Context -> ContextInfo t -> IO t
getContextInfo context ctxInfo =
    case ctxInfo of
      Co.ContextReferenceCount -> stoHelper
      Co.ContextNumDevices     -> stoHelper
      Co.ContextDevices        -> arrHelper
      Co.ContextProperties     -> arrHelper >>= return . Co.CPs

  where

    getter :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
    getter = CL.clGetContextInfo context (Co.value ctxInfo)

    stoHelper :: forall t. Storable t => IO t
    stoHelper = getStorable getter

    arrHelper :: forall t. Storable t => IO [t]
    arrHelper = getArray getter

--
-- Command Queues
--

createCommandQueue :: Context
                   -> Device
                   -> BitSet CommandQueueProperty
                   -> IO CommandQueue
createCommandQueue context device properties =
    handleCLCreationError "createCommandQueue"
    $ CL.clCreateCommandQueue context device (Co.valueFrom properties)

retainCommandQueue :: CommandQueue -> IO ()
retainCommandQueue =
    handleCLError "retainCommandQueue" . CL.clRetainCommandQueue

releaseCommandQueue :: CommandQueue -> IO ()
releaseCommandQueue =
    handleCLError "releaseCommandQueue" . CL.clReleaseCommandQueue

getCommandQueueInfo :: CommandQueue -> CommandQueueInfo t -> IO t
getCommandQueueInfo queue qInfo =
    case qInfo of
        Co.QueueContext        -> stoHelper
        Co.QueueDevice         -> stoHelper
        Co.QueueReferenceCount -> stoHelper
        Co.QueueProperties     -> bitSetHelper

  where

    getter :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
    getter = CL.clGetCommandQueueInfo queue (Co.value qInfo)

    stoHelper :: forall t. Storable t => IO t
    stoHelper = getStorable getter

    bitSetHelper :: forall t u. (Const t u, Integral u, Bits u, Storable u)
                 => IO (BitSet t)
    bitSetHelper = getBitSet getter

--
-- Buffer Objects
--

-- This API is a work-in-progress, let's say.

newtype Buffer t = Buffer { unBuffer :: CLMem }
instance Storable (Buffer t) where
    sizeOf _              = sizeOf (undefined :: CLMem)
    alignment _           = alignment (undefined :: CLMem)
    poke ptr (Buffer mem) = poke (castPtr ptr) mem
    peek _                = error "Can't peek buffers"

createBuffer :: forall t. Storable t
             => Context
             -> BitSet MemFlag
             -> Int
             -> IO (Buffer t)
createBuffer context flags elemCount =
    Buffer `fmap`
    ( handleCLCreationError "createBuffer"
        $ CL.clCreateBuffer context
                            (Co.valueFrom flags)
                            bytesNeeded
                            nullPtr
    )
  where
    bytesNeeded = fromIntegral (elemCount * sizeOf (undefined :: t))

createSubBuffer :: forall t. Storable t
                => Buffer t
                -> BitSet MemFlag
                -> Int
                -> Int
                -> IO (Buffer t)
createSubBuffer (Buffer mem) flags originOfs elemCount =
    do
        alloca
            $ \regionPtr ->
                  do
                      poke regionPtr (Co.CLBufferRegion byteOrigin bytesNeeded)
                      Buffer `fmap`
                          ( handleCLCreationError "createSubBuffer"
                              $ CL.clCreateSubBuffer
                                    mem
                                    (Co.valueFrom flags)
                                    (Co.value Co.CreateTypeRegion)
                                    regionPtr
                          )
  where
      elemSize    = sizeOf (undefined :: t)
      byteOrigin  = fromIntegral $ originOfs * elemSize
      bytesNeeded = fromIntegral $ elemCount * elemSize


-- No point returning the event, as this is a blocking read
enqueueBlockingReadBuffer :: forall t. Storable t
                          => CommandQueue
                          -> Buffer t
                          -> Int
                          -> Int
                          -> [Event]
                          -> IO [t]
enqueueBlockingReadBuffer queue (Buffer mem) originOfs elemCount waitEvents =
    do
        allocaArray elemCount
            $ \arrayPtr ->
                  do
                      _ <- handleCLEventDriven "enqueueBlockingReadBuffer"
                           ( CL.clEnqueueReadBuffer queue
                                                    mem
                                                    True
                                                    byteOrigin
                                                    byteRange
                                                    arrayPtr
                           ) waitEvents
                      peekArray elemCount arrayPtr
  where
      elemSize   = sizeOf (undefined :: t)
      byteOrigin = fromIntegral $ originOfs * elemSize
      byteRange  = fromIntegral $ elemCount * elemSize

enqueueBlockingReadBufferSA :: forall t. Storable t
                            => CommandQueue
                            -> Buffer t
                            -> Int
                            -> Int
                            -> [Event]
                            -> IO (StorableArray Int t)
enqueueBlockingReadBufferSA queue (Buffer mem) originOfs elemCount waitEvents =
    do
        ar <- newArray_ (originOfs, elemCount - 1)
        _ <- withStorableArray ar
             $ \arrayPtr ->
                   handleCLEventDriven "enqueueBlockingReadBufferSA"
                   ( CL.clEnqueueReadBuffer queue
                                            mem
                                            True
                                            byteOrigin
                                            byteRange
                                            arrayPtr
                   ) waitEvents
        return ar
  where
      elemSize   = sizeOf (undefined :: t)
      byteOrigin = fromIntegral $ originOfs * elemSize
      byteRange  = fromIntegral $ elemCount * elemSize

-- No idea if this is the right function or not
enqueueNonBlockingReadBuffer :: forall t. Storable t
                             => CommandQueue
                             -> Buffer t
                             -> Int
                             -> Int
                             -> ([t] -> IO ())
                             -> [Event]
                             -> IO Event
enqueueNonBlockingReadBuffer
    queue
    (Buffer mem)
    originOfs
    elemCount
    continuation
    waitEvents
    =
    do
        arrayPtr <- mallocArray elemCount
        ev <- handleCLEventDriven "enqueueNonBlockingReadBuffer"
              ( CL.clEnqueueReadBuffer queue
                                       mem
                                       False
                                       byteOrigin
                                       byteRange
                                       arrayPtr
              ) waitEvents
        setDatalessEventCallback
            ev
            $ \_ _ _ ->
                  do
                      r <- peekArray elemCount arrayPtr
                      free arrayPtr
                      continuation r
        return ev
  where
      elemSize   = sizeOf (undefined :: t)
      byteOrigin = fromIntegral $ originOfs * elemSize
      byteRange  = fromIntegral $ elemCount * elemSize


-- No point returning the event--see above
enqueueBlockingWriteBuffer :: forall t. Storable t
                           => CommandQueue
                           -> Buffer t
                           -> Int
                           -> [t]
                           -> [Event]
                           -> IO ()
enqueueBlockingWriteBuffer queue (Buffer mem) originOfs elems waitEvents =
    do
        allocaArray (length elems)
            $ \arrayPtr ->
                  do
                      pokeArray arrayPtr elems
                      _ <- handleCLEventDriven "enqueueBlockingWriteBuffer"
                           ( CL.clEnqueueWriteBuffer queue
                                                     mem
                                                     True
                                                     byteOrigin
                                                     byteRange
                                                     arrayPtr
                           ) waitEvents
                      return ()
  where
      elemSize   = sizeOf (undefined :: t)
      byteOrigin = fromIntegral (originOfs * elemSize)
      byteRange  = fromIntegral (length elems * elemSize)

enqueueBlockingWriteBufferSA :: forall t. Storable t
                             => CommandQueue
                             -> Buffer t
                             -> Int
                             -> StorableArray Int t
                             -> [Event]
                             -> IO Event
enqueueBlockingWriteBufferSA queue (Buffer mem) originOfs elems waitEvents =
    do
        (low, high) <- getBounds elems
        withStorableArray elems
            $ \arrayPtr ->
                  handleCLEventDriven "enqueueBlockingWriteBufferSA"
                  ( CL.clEnqueueWriteBuffer queue
                                            mem
                                            True
                                            byteOrigin
                                            (byteRange (high - low + 1))
                                            arrayPtr
                  ) waitEvents
  where
      elemSize        = sizeOf (undefined :: t)
      byteOrigin      = fromIntegral (originOfs * elemSize)
      byteRange count = fromIntegral (count * elemSize)

enqueueNonBlockingWriteBuffer :: forall t. Storable t
                              => CommandQueue
                              -> Buffer t
                              -> Int
                              -> [t]
                              -> [Event]
                              -> IO Event
enqueueNonBlockingWriteBuffer queue (Buffer mem) originOfs elems waitEvents =
    do
        arrayPtr <- newArray elems
        ev <- handleCLEventDriven "enqueueNonBlockingWriteBuffer"
              ( CL.clEnqueueWriteBuffer queue
                                        mem
                                        False
                                        byteOrigin
                                        byteRange
                                        arrayPtr
              ) waitEvents
        setDatalessEventCallback ev (\_ _ _ -> free arrayPtr)
        return ev
  where
      elemSize   = sizeOf (undefined :: t)
      byteOrigin = fromIntegral (originOfs * elemSize)
      byteRange  = fromIntegral (length elems * elemSize)

-- TODO: enqueueReadBufferRect and enqueueWriteBufferRect, as I don't
-- understand them

enqueueCopyBuffer :: forall t. Storable t
                  => CommandQueue
                  -> Buffer t
                  -> Buffer t
                  -> Int
                  -> Int
                  -> Int
                  -> [Event]
                  -> IO Event
enqueueCopyBuffer queue (Buffer mem0) (Buffer mem1) origin0 origin1 nElems =
    handleCLEventDriven "enqueueCopyBuffer"
                        $ CL.clEnqueueCopyBuffer queue
                                                 mem0
                                                 mem1
                                                 byteOrigin0
                                                 byteOrigin1
                                                 byteRange
  where
    elemSize    = sizeOf (undefined :: t)
    byteOrigin0 = fromIntegral $ origin0 * elemSize
    byteOrigin1 = fromIntegral $ origin1 * elemSize
    byteRange   = fromIntegral $ nElems * elemSize


-- TODO: enqueueCopyBufferRect: see above whining about rects

retainBuffer :: Buffer t -> IO ()
retainBuffer (Buffer mem) =
    handleCLError "retainBuffer" (CL.clRetainMemObject mem)

releaseBuffer :: Buffer t -> IO ()
releaseBuffer (Buffer mem) =
    handleCLError "releaseBuffer" (CL.clReleaseMemObject mem)

getMemObjectInfo :: CLMem -> MemInfo u -> IO u
getMemObjectInfo mem memInfo =
    case memInfo of
        Co.MemType                -> constEnumHelper
        Co.MemFlags               -> bitSetHelper
        Co.MemSize                -> stoHelper
        Co.MemHostPtr             -> stoHelper
        Co.MemMapCount            -> stoHelper
        Co.MemReferenceCount      -> stoHelper
        Co.MemContext             -> stoHelper
        Co.MemAssociatedMemObject -> stoHelper
        Co.MemOffset              -> stoHelper
  where

    getter :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
    getter = CL.clGetMemObjectInfo mem (Co.value memInfo)

    stoHelper :: forall t. Storable t => IO t
    stoHelper = getStorable getter

    bitSetHelper :: forall t u. (Const t u, Integral u, Bits u, Storable u)
                 => IO (BitSet t)
    bitSetHelper = getBitSet getter

    constEnumHelper :: forall t u. ( Enum t
                                   , Bounded t
                                   , Const t u
                                   , Eq u
                                   , Storable u
                                   , Show u
                                   )
                    => IO t
    constEnumHelper = getConstEnum getter


getBufferInfo :: Buffer t -> MemInfo u -> IO u
getBufferInfo (Buffer mem) = getMemObjectInfo mem

--
-- Images
--

data Image t = Image { imageMemObject  :: CLMem
                     , imageRowPitch   :: SizeT
                     , imageSlicePitch :: SizeT }

create2DImage :: forall t. Storable t
              => Context
              -> BitSet MemFlag
              -> CLImageFormat
              -> Int
              -> Int
              -> IO (Image t)
create2DImage context flags format width height =
    alloca
    $ \formatPtr ->
          do
              poke formatPtr format
              mem <- handleCLCreationError "create2DImage"
                     $ CL.clCreateImage2D context
                                          (Co.valueFrom flags)
                                          formatPtr
                                          byteWidth
                                          byteHeight
                                          0
                                          nullPtr
              return (Image mem byteWidth 0)
  where
      elemSize   = sizeOf (undefined :: t)
      byteWidth  = fromIntegral $ elemSize * width
      byteHeight = fromIntegral $ elemSize * height

create3DImage :: forall t. Storable t
              => Context
              -> BitSet MemFlag
              -> CLImageFormat
              -> Int
              -> Int
              -> Int
              -> IO (Image t)
create3DImage context flags format width height depth =
    alloca
    $ \formatPtr ->
          do
              poke formatPtr format
              mem <- handleCLCreationError "create3DImage"
                     $ CL.clCreateImage3D context
                                          (Co.valueFrom flags)
                                          formatPtr
                                          byteWidth
                                          byteHeight
                                          byteDepth
                                          0
                                          0
                                          nullPtr
              return (Image mem byteWidth (byteWidth * byteHeight))
  where
      elemSize   = sizeOf (undefined :: t)
      byteWidth  = fromIntegral $ elemSize * width
      byteHeight = fromIntegral $ elemSize * height
      byteDepth  = fromIntegral $ elemSize * depth

getSupportedImageFormats :: Context
                         -> BitSet MemFlag
                         -> MemObjectType
                         -> IO [CLImageFormat]
getSupportedImageFormats context flags objType =
    getCountedArray
    $ CL.clGetSupportedImageFormats context
                                    (Co.valueFrom flags)
                                    (Co.value objType)

type Point = (Int, Int, Int)

allocaTriple :: Storable t => (t,t,t) -> (Ptr t -> IO r) -> IO r
allocaTriple (x,y,z) c =
    allocaArray 3 $ \ triplePtr ->
        do pokeArray triplePtr [x,y,z]
           c triplePtr

fit :: (Num a, Num b, Num c) => (Int, Int, Int) -> (a, b, c)
fit (x,y,z) = (fromIntegral x, fromIntegral y, fromIntegral z)

-- TODO: [t] seems like absolutely the wrong return type, but I'm not sure what
-- would be better. Adjust this when I have actual applications.
enqueueBlockingReadImage :: forall t. Storable t
                         => CommandQueue
                         -> Image t
                         -> Point
                         -> Point
                         -> [Event]
                         -> IO [t]
enqueueBlockingReadImage
    queue
    (Image mem rowPitch slicePitch)
    originPoint
    rangePoint@(dx, dy, dz)
    waitEvents
    =
    allocaTriple (fit originPoint)
    $ \originPtr ->
          allocaTriple (fit rangePoint)
          $ \rangePtr ->
                allocaArray resultCount
                $ \arrayPtr ->
                      do
                          _ <- handleCLEventDriven "enqueueBlockingReadImage"
                              ( CL.clEnqueueReadImage queue
                                                      mem
                                                      True
                                                      originPtr
                                                      rangePtr
                                                      rowPitch
                                                      slicePitch
                                                      arrayPtr
                              ) waitEvents
                          peekArray resultCount arrayPtr
  where resultCount = dx * dy * dz

enqueueNonBlockingReadImage :: forall t. Storable t
                            => CommandQueue
                            -> Image t
                            -> Point
                            -> Point
                            -> ([t] -> IO ())
                            -> [Event]
                            -> IO Event
enqueueNonBlockingReadImage
    queue
    (Image mem rowPitch slicePitch)
    originPoint
    rangePoint@(dx, dy, dz)
    continuation
    waitEvents
    =
    allocaTriple (fit originPoint)
    $ \originPtr ->
          allocaTriple (fit rangePoint)
          $ \rangePtr ->
                do
                    arrayPtr <- mallocArray resultCount
                    ev <- handleCLEventDriven "enqueueNonBlockingReadImage"
                          ( CL.clEnqueueReadImage queue
                                                  mem
                                                  False
                                                  originPtr
                                                  rangePtr
                                                  rowPitch
                                                  slicePitch
                                                  arrayPtr
                          ) waitEvents
                    setDatalessEventCallback
                        ev
                        ( \_ _ _ -> do
                                r <- peekArray resultCount arrayPtr
                                free arrayPtr
                                continuation r
                        )
                    return ev
    where resultCount = dx * dy * dz


-- Again, [t] is awful here
enqueueBlockingWriteImage :: forall t. Storable t
                          => CommandQueue
                          -> Image t
                          -> Point
                          -> Point
                          -> [t]
                          -> [Event]
                          -> IO ()
enqueueBlockingWriteImage
    queue
    (Image mem rowPitch slicePitch)
    originPoint
    rangePoint@(dx, dy, dz)
    elems
    waitEvents

    | length elems < resultCount =
          userFail "Not enough elements passed to enqueueBlockingWriteImage" []
    | otherwise =
          allocaTriple (fit originPoint)
          $ \originPtr ->
                allocaTriple (fit rangePoint)
                $ \rangePtr ->
                      allocaArray resultCount
                      $ \arrayPtr ->
                            do
                                pokeArray arrayPtr (take resultCount elems)
                                _ <- handleCLEventDriven
                                     "enqueueBlockingWriteImage"
                                     ( CL.clEnqueueWriteImage queue
                                                              mem
                                                              True
                                                              originPtr
                                                              rangePtr
                                                              rowPitch
                                                              slicePitch
                                                              arrayPtr
                                     ) waitEvents
                                return ()
  where resultCount = dx * dy * dz

enqueueNonblockingWriteImage :: forall t. Storable t
                             => CommandQueue
                             -> Image t
                             -> Point
                             -> Point
                             -> [t]
                             -> [Event]
                             -> IO Event
enqueueNonblockingWriteImage
    queue
    (Image mem rowPitch slicePitch)
    originPoint
    rangePoint@(dx, dy, dz)
    elems
    waitEvents

    | length elems < resultCount =
      userFail "Not enough elements passed to enqueueBlockingWriteImage" []
    | otherwise =
        allocaTriple (fit originPoint)
        $ \originPtr ->
              allocaTriple (fit rangePoint)
              $ \rangePtr ->
                    do
                        arrayPtr <- newArray (take resultCount elems)
                        ev <- handleCLEventDriven "enqueueNonblockingWriteImage"
                              ( CL.clEnqueueWriteImage queue
                                                       mem
                                                       True
                                                       originPtr
                                                       rangePtr
                                                       rowPitch
                                                       slicePitch
                                                       arrayPtr
                              ) waitEvents
                        setDatalessEventCallback ev (\_ _ _ -> free arrayPtr)
                        return ev
  where
      resultCount = dx * dy * dz

enqueueCopyImage :: CommandQueue
                 -> Image t
                 -> Image t
                 -> Point
                 -> Point
                 -> Point
                 -> [Event]
                 -> IO Event
enqueueCopyImage
    queue
    (Image mem0 _ _)
    (Image mem1 _ _)
    origin0
    origin1
    rangePoint
    waitEvents
    =
    allocaTriple (fit origin0)
    $ \origin0Ptr ->
          allocaTriple (fit origin1)
          $ \origin1Ptr ->
                allocaTriple (fit rangePoint)
                $ \rangePtr ->
                      handleCLEventDriven "enqueueCopyImage"
                      ( CL.clEnqueueCopyImage queue
                                              mem0
                                              mem1
                                              origin0Ptr
                                              origin1Ptr
                                              rangePtr
                      ) waitEvents

enqueueCopyImageToBuffer :: CommandQueue
                         -> Image t
                         -> Buffer t
                         -> Point
                         -> Point
                         -> Int
                         -> [Event]
                         -> IO Event
enqueueCopyImageToBuffer
    queue
    (Image mem0 _ _)
    (Buffer mem1)
    origin0
    rangePoint
    origin1
    waitEvents
    =
    allocaTriple (fit origin0)
    $ \origin0Ptr ->
          allocaTriple (fit rangePoint)
          $ \rangePtr ->
                handleCLEventDriven "enqueueCopyImageToBuffer"
                ( CL.clEnqueueCopyImageToBuffer queue
                                                mem0
                                                mem1
                                                origin0Ptr
                                                rangePtr
                                                (fromIntegral origin1)
                ) waitEvents

enqueueCopyBufferToImage :: CommandQueue
                         -> Buffer t
                         -> Image t
                         -> Int
                         -> Point
                         -> Point
                         -> [Event]
                         -> IO Event
enqueueCopyBufferToImage
    queue
    (Buffer mem0)
    (Image mem1 _ _)
    origin0
    origin1
    rangePoint
    waitEvents
    =
    allocaTriple (fit origin1)
    $ \origin1Ptr ->
          allocaTriple (fit rangePoint)
          $ \rangePtr ->
                handleCLEventDriven "enqueueCopyBufferToImage"
                ( CL.clEnqueueCopyBufferToImage queue
                                                mem0
                                                mem1
                                                (fromIntegral origin0)
                                                origin1Ptr
                                                rangePtr
                ) waitEvents

getImageMemObjectInfo :: Image t -> MemInfo u -> IO u
getImageMemObjectInfo (Image img _ _) = getMemObjectInfo img

getImageInfo :: Image t -> ImageInfo u -> IO u
getImageInfo (Image mem _ _) imgInfo =
    -- type inference is made of win and awesome
    case imgInfo of
        Co.ImageFormat      -> get
        Co.ImageElementSize -> get
        Co.ImageRowPitch    -> get
        Co.ImageSlicePitch  -> get
        Co.ImageWidth       -> get
        Co.ImageDepth       -> get
        Co.ImageHeight      -> get
  where
    get :: forall t. Storable t => IO t
    get = getStorable (CL.clGetImageInfo mem (Co.value imgInfo))

retainImage, releaseImage :: Image t -> IO ()
retainImage (Image img _ _) =
    handleCLError "retainImage" (CL.clRetainMemObject img)
releaseImage (Image img _ _) =
    handleCLError "releaseImage" (CL.clReleaseMemObject img)

-- TODO: Samplers

createProgramWithSource :: Context -> [String] -> IO Program
createProgramWithSource context sources =
    allocaArray nSources
    $ \sourceStrings ->
          do
              mapM_ ( \(str, n) ->
                          newCString str >>=
                          pokeElemOff sourceStrings n
                    ) (zip sources [0..nSources])
              p <- handleCLCreationError "createProgramWithSource"
                   $ CL.clCreateProgramWithSource context
                                                  (fromIntegral nSources)
                                                  sourceStrings
                                                  nullPtr
              mapM_ (\n -> peekElemOff sourceStrings n >>= free) [0..nSources]
              return p
  where
    nSources = length sources

-- TODO: createProgramWithBinary

retainProgram :: Program -> IO ()
retainProgram = handleCLError "retainProgram" . CL.clRetainProgram

releaseProgram :: Program -> IO ()
releaseProgram = handleCLError "releaseProgram" . CL.clReleaseProgram

buildProgram :: Program -> [Device] -> String -> IO ()
buildProgram prog devices options =
    withArray devices
    $ \devArray ->
          withCString options
          $ \optStr ->
                handleCLError "buildProgram"
                $ CL.clBuildProgram prog
                                    (fromIntegral $ length devices)
                                    devArray
                                    optStr
                                    nullFunPtr
                                    nullPtr

-- Ignoring the callback parameters since we can take advantage of scoping and
-- closures when building the callback.
nonBlockingBuildProgram :: Program -> [Device] -> String -> IO () -> IO ()
nonBlockingBuildProgram prog devices options callback =
    withArray devices
    $ \devArray ->
          withCString options
          $ \optStr ->
                do
                    callback' <- CL.makeBuildProgramCallback
                                 $ const (const callback)
                    handleCLError "nonBlockingBuildProgram"
                        $ CL.clBuildProgram prog
                                            (fromIntegral $ length devices)
                                            devArray
                                            optStr
                                            callback'
                                            nullPtr

unloadCompiler :: IO ()
unloadCompiler = CL.clUnloadCompiler

getProgramInfo :: Program -> ProgramInfo t -> IO t
getProgramInfo program progInfo =
    case progInfo of
        Co.ProgramReferenceCount -> stoHelper
        Co.ProgramContext        -> stoHelper
        Co.ProgramNumDevices     -> stoHelper
        Co.ProgramDevices        -> arrayHelper
        Co.ProgramSource         -> stringHelper
        Co.ProgramBinarySizes    -> arrayHelper
        Co.ProgramBinaries       -> arrayHelper
  where

    getter :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
    getter = CL.clGetProgramInfo program (Co.value progInfo)

    stoHelper :: forall t. Storable t => IO t
    stoHelper = getStorable getter

    stringHelper :: IO String
    stringHelper = getString getter

    arrayHelper :: forall t. Storable t => IO [t]
    arrayHelper = getArray getter

getProgramBuildInfo :: Program -> Device -> ProgramBuildInfo t -> IO t
getProgramBuildInfo program device progBuildInfo =
    case progBuildInfo of
        Co.ProgramBuildStatus  -> constEnumHelper
        Co.ProgramBuildOptions -> stringHelper
        Co.ProgramBuildLog     -> stringHelper
  where

    getter :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
    getter = CL.clGetProgramBuildInfo program device (Co.value progBuildInfo)

    constEnumHelper :: forall t u. ( Enum t
                                   , Bounded t
                                   , Const t u
                                   , Eq u
                                   , Storable u
                                   , Show u
                                   )
                    => IO t
    constEnumHelper = getConstEnum getter

    stringHelper :: IO String
    stringHelper = getString getter

createKernel :: Program -> String -> IO Kernel
createKernel program name =
    withCString name
    $ \cName ->
          handleCLCreationError "createKernel" $ CL.clCreateKernel program cName

createKernelsInProgram :: Program -> IO [Kernel]
createKernelsInProgram program =
    getCountedArray (CL.clCreateKernelsInProgram program)

retainKernel :: Kernel -> IO ()
retainKernel = handleCLError "retainKernel" . CL.clRetainKernel

releaseKernel :: Kernel -> IO ()
releaseKernel = handleCLError "releaseKernel" . CL.clReleaseKernel

setKernelArg :: forall t. Storable t => Kernel -> Int -> t -> IO ()
setKernelArg kernel n val =
    alloca
    $ \valPtr ->
          do
              poke valPtr val
              handleCLError "setKernelArg"
                  $ CL.clSetKernelArg kernel
                                      (fromIntegral n)
                                      (fromIntegral (sizeOf (undefined :: t)))
                                      valPtr

setKernelArgL :: forall t. Storable t => Kernel -> Int -> [t] -> IO ()
setKernelArgL kernel n vals =
    withArray vals
    $ handleCLError "setKernelArgL"
    . CL.clSetKernelArg kernel (fromIntegral n) valsSize
  where
    valsSize = fromIntegral $ length vals * sizeOf (undefined :: t)

getKernelInfo :: Kernel -> KernelInfo t -> IO t
getKernelInfo kernel kInfo =
    case kInfo of
        Co.KernelFunctionName   -> stringHelper
        Co.KernelNumArgs        -> stoHelper
        Co.KernelReferenceCount -> stoHelper
        Co.KernelContext        -> stoHelper
        Co.KernelProgram        -> stoHelper
  where

    getter :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
    getter = CL.clGetKernelInfo kernel (Co.value kInfo)

    stringHelper :: IO String
    stringHelper = getString getter

    stoHelper :: forall t. Storable t => IO t
    stoHelper = getStorable getter

getKernelWorkGroupInfo :: Kernel -> Device -> KernelWorkGroupInfo t -> IO t
getKernelWorkGroupInfo kernel device kwgInfo =
    case kwgInfo of
        Co.KernelWorkGroupSize                  -> stoHelper
        Co.KernelCompileWorkGroupSize           -> arrayHelper
        Co.KernelLocalMemSize                   -> stoHelper
        Co.KernelPreferredWorkGroupSizeMultiple -> stoHelper
        Co.KernelPrivateMemSize                 -> stoHelper
  where

    getter :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
    getter = CL.clGetKernelWorkGroupInfo kernel device (Co.value kwgInfo)

    stoHelper :: forall t. Storable t => IO t
    stoHelper = getStorable getter

    arrayHelper :: forall t. Storable t => IO [t]
    arrayHelper = getArray getter

enqueueNDRangeKernel :: CommandQueue
                     -> Kernel
                     -> [Int]
                     -> [Int]
                     -> [Int]
                     -> [Event]
                     -> IO Event
enqueueNDRangeKernel queue k offset global local waitEvents =
    withArray' (map fromIntegral offset)
    $ \offsetA ->
          withArray' (map fromIntegral global)
          $ \globalA ->
                withArray' (map fromIntegral local)
                $ \localA ->
                      handleCLEventDriven "enqueueNDRangeKernel"
                      ( CL.clEnqueueNDRangeKernel queue
                                                  k
                                                  (fromIntegral $ length global)
                                                  offsetA
                                                  globalA
                                                  localA
                      ) waitEvents
  where
    withArray' [] c = c nullPtr
    withArray' xs c = withArray xs c

enqueueTask :: CommandQueue -> Kernel -> [Event] -> IO Event
enqueueTask queue k =
    handleCLEventDriven "enqueueTask" (CL.clEnqueueTask queue k)

enqueueNativeKernel :: CommandQueue -> IO () -> [Event] -> IO Event
enqueueNativeKernel queue k waitEvents =
    do
        k' <- CL.makeNativeKernel k
        handleCLEventDriven "enqueueNativeKernel"
            ( CL.clEnqueueNativeKernel queue
                                       k'
                                       nullPtr
                                       0
                                       0
                                       nullPtr
                                       nullPtr
            ) waitEvents

--
-- Events
--

createUserEvent :: Context -> IO Event
createUserEvent =
    handleCLCreationError "createUserEvent" . CL.clCreateUserEvent

setUserEventStatus :: Event -> EventStatus -> IO ()
setUserEventStatus event =
    handleCLError "setUserEventStatus"
    . CL.clSetUserEventStatus event
    . Co.value

waitForEvents :: [Event] -> IO ()
waitForEvents events =
    withArray events
    $ \evArray ->
          handleCLError "waitForEvents"
          $ CL.clWaitForEvents (fromIntegral $ length events) evArray

getEventInfo :: Event -> EventInfo t -> IO t
getEventInfo event evInfo =
    case evInfo of
        Co.EventCommandQueue           -> stoHelper
        Co.EventContext                -> stoHelper
        Co.EventCommandType            -> constEnumHelper
        Co.EventCommandExecutionStatus ->
            Co.eventStatusFromCLInt `fmap` stoHelper
        Co.EventReferenceCount         -> stoHelper
  where

    getter :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
    getter = CL.clGetEventInfo event (Co.value evInfo)

    stoHelper :: forall t. Storable t => IO t
    stoHelper = getStorable getter

    constEnumHelper :: forall t u. ( Enum t
                                   , Bounded t
                                   , Const t u
                                   , Eq u
                                   , Storable u
                                   , Show u
                                   )
                    => IO t
    constEnumHelper = getConstEnum getter

eventCallback :: EventCallback a -> CLEventCallback a
eventCallback callback event clint =
    callback event (Co.eventStatusFromCLInt clint)

setEventCallback :: Storable a => Event -> EventCallback a -> a -> IO ()
setEventCallback event callBack userData =
    do
        dataPtr <- malloc
        poke dataPtr userData
        callbackPtr <- CL.makeEventCallback (eventCallback callBack)
        handleCLError "setEventCallback"
            $ CL.clSetEventCallback event 0 callbackPtr dataPtr

setDatalessEventCallback :: Event -> EventCallback t -> IO ()
setDatalessEventCallback event callBack =
    do
        callbackPtr <- CL.makeEventCallback (eventCallback callBack)
        handleCLError "setDatalessEventCallback"
            $ CL.clSetEventCallback event 0 callbackPtr nullPtr


retainEvent :: Event -> IO ()
retainEvent = handleCLError "retainEvent" . CL.clRetainEvent

releaseEvent :: Event -> IO ()
releaseEvent = handleCLError "releaseEvent" . CL.clReleaseEvent

enqueueMarker :: CommandQueue -> IO Event
enqueueMarker queue =
    alloca
    $ \evPtr ->
          do
              handleCLError "enqueueMarker" (CL.clEnqueueMarker queue evPtr)
              peek evPtr

enqueueBarrier :: CommandQueue -> IO ()
enqueueBarrier = handleCLError "enqueueBarrier" . CL.clEnqueueBarrier

enqueueWaitForEvents :: CommandQueue -> [Event] -> IO ()
enqueueWaitForEvents queue events =
    withArray events
    $ \evArray ->
          handleCLError "enqueueWaitForEvents"
          $ CL.clEnqueueWaitForEvents queue
                                      (fromIntegral $ length events)
                                      evArray

getEventProfilingInfo :: Event -> ProfilingInfo t -> IO t
getEventProfilingInfo event profInfo =
    case profInfo of
        Co.ProfilingCommandQueued -> get
        Co.ProfilingCommandSubmit -> get
        Co.ProfilingCommandStart  -> get
        Co.ProfilingCommandEnd    -> get
  where
    get :: forall t. Storable t => IO t
    get = getStorable (CL.clGetEventProfilingInfo event (Co.value profInfo))

flush :: CommandQueue -> IO ()
flush = handleCLError "flush" . CL.clFlush

finish :: CommandQueue -> IO ()
finish = handleCLError "finish" . CL.clFinish
