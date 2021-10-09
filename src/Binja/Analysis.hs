module Binja.Analysis (
  module Exports,
getParametersForAnalysis,setParametersForAnalysis) where

import Binja.C.Main (BNBinaryView)
import Binja.C.Util (allocAndPeek)
import qualified Binja.Core as BN
import Binja.Prelude
import Binja.Types.Analysis as Exports
import Foreign (Storable (poke), alloca)

getParametersForAnalysis :: BNBinaryView -> IO BNAnalysisParameters
getParametersForAnalysis bv = allocAndPeek $ BN.wrapBNGetParametersForAnalysis bv

setParametersForAnalysis :: BNBinaryView -> BNAnalysisParameters -> IO ()
setParametersForAnalysis bv params = alloca $ \ptr -> do
  poke ptr params
  BN.wrapBNSetParametersForAnalysis bv ptr