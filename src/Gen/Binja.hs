{-# LANGUAGE ForeignFunctionInterface #-}
-- {-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Gen.Binja where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String
import Foreign.ForeignPtr

-- generated Binja bindings
-- (not really generated at the moment...)

data BNDataBuffer
data BNBinaryView
data BNBinaryViewType
data BNBinaryReader
data BNBinaryWriter
data BNFileMetadata
data BNTransform
data BNArchitecture
data BNFunction
data BNBasicBlock
data BNDownloadProvider
data BNDownloadInstance
data BNFlowGraph
data BNFlowGraphNode
data BNFlowGraphLayoutRequest
data BNSymbol
data BNTemporaryFile
data BNLowLevelILFunction
data BNMediumLevelILFunction
data BNType
data BNStructure
data BNNamedTypeReference
data BNEnumeration
data BNCallingConvention
data BNPlatform
data BNAnalysisCompletionEvent
data BNDisassemblySettings
data BNScriptingProvider
data BNScriptingInstance
data BNMainThreadAction
data BNBackgroundTask
data BNRepository
data BNRepoPlugin
data BNRepositoryManager
data BNMetadata
data BNReportCollection
data BNRelocation
data BNSegment
data BNSection
data BNRelocationInfo
data BNRelocationHandler
data BNDataRenderer
data BNDataRendererContainer

foreign import ccall unsafe "/tmp/beauty/binaryninjacore.h BNCreateFileMetadata"
  createFileMetadata :: IO (Ptr BNFileMetadata)

