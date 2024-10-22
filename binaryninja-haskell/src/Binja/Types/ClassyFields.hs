{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.ClassyFields where

import Control.Lens (makeFieldsNoPrefix)

-- this generates type classes for all the fields using in whole lib
-- so they can be unified under the same HasX typeclasses

data DummyRecordThatIsNeverUsed = DummyRecordThatIsNeverUsed
  { _confidence :: ()
  , _typeClass :: ()
  , _width :: ()
  , _alignment :: ()
  , _signed :: ()
  , _signedConfidence :: ()
  , _isConst :: ()
  , _constConfidence :: ()
  , _sourceType :: ()
  , _index :: ()
  , _storage :: ()
  , _name :: ()
  , _varType :: ()
  , _bnType :: ()
  , _value :: ()
  , _handle :: ()
  , _start :: ()
  , _func :: ()
  , _semClasses :: ()
  , _semGroups :: ()
  , _end :: ()
  , _operation :: ()
  , _sourceOperand :: ()
  , _size :: ()
  , _operands :: ()
  , _address :: ()
  , _exprIndex :: ()
  , _opData :: ()
  , _op :: ()
  , _var :: ()
  , _version :: ()
  , _dest :: ()
  , _src :: ()
  , _offset :: ()
  , _high :: ()
  , _low :: ()
  , _constant :: ()
  , _left :: ()
  , _right :: ()
  , _carry :: ()
  , _targets :: ()
  , _params :: ()
  , _output :: ()
  , _stack :: ()
  , _condition :: ()
  , _true :: ()
  , _false :: ()
  , _vector :: ()
  , _intrinsic :: ()
  , _prev :: ()
  , _src_memory :: ()
  , _dest_memory :: ()
  , _tgt :: ()
  , _branchType :: ()
  , _isBackEdge :: ()
  , _isFallThrough :: ()
  }

$(makeFieldsNoPrefix ''DummyRecordThatIsNeverUsed)
