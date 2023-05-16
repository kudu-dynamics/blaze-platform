module Blaze.Types.Pil.Common where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Function (Function)
import Blaze.Types.Graph (Identifiable (getNodeId), NodeId (NodeId))


newtype StmtIndex = StmtIndex { val :: Int }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype Num
  deriving anyclass (Hashable, ToJSON, FromJSON)

type Symbol = Text

-- TODO: should this be in Bits?
newtype Size a = Size Bytes
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, ByteBased)
  deriving anyclass (Hashable, ToJSON, FromJSON)

newtype CtxId = CtxId Word64
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)
  deriving newtype (Num, Real, Enum, Integral)

data Ctx = Ctx
  { func :: Function
  , ctxId :: CtxId
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- Maybe is used to wrap _func and _ctxId since
-- contextual information may not be available or desirable
-- when introducing "synthetic" variables. (I.e., variables
-- which do not correspond to variables in the source program.)
data PilVar = PilVar
  { size :: Size PilVar
    -- TODO: Reassess use of Maybe for ctx.
    --       Currently needed when introducing synthesized PilVars
    --       when replacing store statements. May also be useful for
    --       introducing arbitrary symbols used in constraints?
    --       Another option is to explicitly use a default context
    --       Related to this is having Blaze.Pil.Construct functions
    --       play nice with context management.
  , ctx :: Maybe Ctx
  , symbol :: Symbol
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

instance Identifiable PilVar Int where
  getNodeId pv = NodeId $ hash pv

newtype Storage = Storage
  { label :: Label
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

data StackOffset = StackOffset
  { ctx :: Ctx
  , offset :: ByteOffset
  } deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, FromJSON)

type Keyword = Text

data Label = StackOffsetLabel StackOffset
           | KeywordLabel Keyword
           deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, FromJSON)
