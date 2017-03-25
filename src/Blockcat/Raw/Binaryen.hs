{-# LANGUAGE InterruptibleFFI #-}

module Blockcat.Raw.Binaryen where

import Foreign.C

newtype BinaryenType = BinaryenType
    { getBinaryenType :: CUInt
    }

foreign import ccall interruptible "binaryen-c.h BinaryenInt32"
               binaryenInt32 :: BinaryenType

foreign import ccall interruptible "binaryen-c.h BinaryenInt64"
               binaryenInt64 :: BinaryenType

foreign import ccall interruptible "binaryen-c.h BinaryenFloat32"
               binaryenFloat32 :: BinaryenType

foreign import ccall interruptible "binaryen-c.h BinaryenFloat64"
               binaryenFloat64 :: BinaryenType
