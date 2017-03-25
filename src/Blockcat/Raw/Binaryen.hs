{-# LANGUAGE InterruptibleFFI #-}

module Blockcat.Raw.Binaryen where

import Foreign.C

newtype BinaryenType = BinaryenType
    { getBinaryenType :: CUInt
    }

foreign import ccall interruptible "binaryen-c.h BinaryenInt32"
               c_BinaryenInt32 :: BinaryenType

foreign import ccall interruptible "binaryen-c.h BinaryenInt64"
               c_BinaryenInt64 :: BinaryenType

foreign import ccall interruptible "binaryen-c.h BinaryenFloat32"
               c_BinaryenFloat32 :: BinaryenType

foreign import ccall interruptible "binaryen-c.h BinaryenFloat64"
               c_BinaryenFloat64 :: BinaryenType

newtype BinaryenOp = BinaryenOp
    { getBinaryenOp :: CInt
    }

foreign import ccall interruptible "binaryen-c.h BinaryenClzInt32"
               c_BinaryenClzInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenCtzInt32"
               c_BinaryenCtzInt32 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenPopcntInt32" c_BinaryenPopcntInt32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenNegFloat32" c_BinaryenNegFloat32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenAbsFloat32" c_BinaryenAbsFloat32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenCeilFloat32" c_BinaryenCeilFloat32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenFloorFloat32" c_BinaryenFloorFloat32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenTruncFloat32" c_BinaryenTruncFloat32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenNearestFloat32" c_BinaryenNearestFloat32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenSqrtFloat32" c_BinaryenSqrtFloat32 ::
               BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenEqZInt32"
               c_BinaryenEqZInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenClzInt64"
               c_BinaryenClzInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenCtzInt64"
               c_BinaryenCtzInt64 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenPopcntInt64" c_BinaryenPopcntInt64 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenNegFloat64" c_BinaryenNegFloat64 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenAbsFloat64" c_BinaryenAbsFloat64 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenCeilFloat64" c_BinaryenCeilFloat64 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenFloorFloat64" c_BinaryenFloorFloat64 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenTruncFloat64" c_BinaryenTruncFloat64 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenNearestFloat64" c_BinaryenNearestFloat64 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenSqrtFloat64" c_BinaryenSqrtFloat64 ::
               BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenEqZInt64"
               c_BinaryenEqZInt64 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenExtendSInt32" c_BinaryenExtendSInt32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenExtendUInt32" c_BinaryenExtendUInt32 ::
               BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenWrapInt64"
               c_BinaryenWrapInt64 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenTruncSFloat32ToInt32"
               c_BinaryenTruncSFloat32ToInt32 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenTruncSFloat32ToInt64"
               c_BinaryenTruncSFloat32ToInt64 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenTruncUFloat32ToInt32"
               c_BinaryenTruncUFloat32ToInt32 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenTruncUFloat32ToInt64"
               c_BinaryenTruncUFloat32ToInt64 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenTruncSFloat64ToInt32"
               c_BinaryenTruncSFloat64ToInt32 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenTruncSFloat64ToInt64"
               c_BinaryenTruncSFloat64ToInt64 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenTruncUFloat64ToInt32"
               c_BinaryenTruncUFloat64ToInt32 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenTruncUFloat64ToInt64"
               c_BinaryenTruncUFloat64ToInt64 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenReinterpretFloat32"
               c_BinaryenReinterpretFloat32 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenReinterpretFloat64"
               c_BinaryenReinterpretFloat64 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenConvertSInt32ToFloat32"
               c_BinaryenConvertSInt32ToFloat32 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenConvertSInt32ToFloat64"
               c_BinaryenConvertSInt32ToFloat64 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenConvertUInt32ToFloat32"
               c_BinaryenConvertUInt32ToFloat32 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenConvertUInt32ToFloat64"
               c_BinaryenConvertUInt32ToFloat64 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenConvertSInt64ToFloat32"
               c_BinaryenConvertSInt64ToFloat32 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenConvertSInt64ToFloat64"
               c_BinaryenConvertSInt64ToFloat64 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenConvertUInt64ToFloat32"
               c_BinaryenConvertUInt64ToFloat32 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenConvertUInt64ToFloat64"
               c_BinaryenConvertUInt64ToFloat64 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenPromoteFloat32" c_BinaryenPromoteFloat32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenDemoteFloat64" c_BinaryenDemoteFloat64 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenReinterpretInt32" c_BinaryenReinterpretInt32
               :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenReinterpretInt64" c_BinaryenReinterpretInt64
               :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenAddInt32"
               c_BinaryenAddInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenSubInt32"
               c_BinaryenSubInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenMulInt32"
               c_BinaryenMulInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenDivSInt32"
               c_BinaryenDivSInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenDivUInt32"
               c_BinaryenDivUInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenRemSInt32"
               c_BinaryenRemSInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenRemUInt32"
               c_BinaryenRemUInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenAndInt32"
               c_BinaryenAndInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenOrInt32"
               c_BinaryenOrInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenXorInt32"
               c_BinaryenXorInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenShlInt32"
               c_BinaryenShlInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenShrUInt32"
               c_BinaryenShrUInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenShrSInt32"
               c_BinaryenShrSInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenRotLInt32"
               c_BinaryenRotLInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenRotRInt32"
               c_BinaryenRotRInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenEqInt32"
               c_BinaryenEqInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenNeInt32"
               c_BinaryenNeInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenLtSInt32"
               c_BinaryenLtSInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenLtUInt32"
               c_BinaryenLtUInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenLeSInt32"
               c_BinaryenLeSInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenLeUInt32"
               c_BinaryenLeUInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenGtSInt32"
               c_BinaryenGtSInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenGtUInt32"
               c_BinaryenGtUInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenGeSInt32"
               c_BinaryenGeSInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenGeUInt32"
               c_BinaryenGeUInt32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenAddInt64"
               c_BinaryenAddInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenSubInt64"
               c_BinaryenSubInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenMulInt64"
               c_BinaryenMulInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenDivSInt64"
               c_BinaryenDivSInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenDivUInt64"
               c_BinaryenDivUInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenRemSInt64"
               c_BinaryenRemSInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenRemUInt64"
               c_BinaryenRemUInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenAndInt64"
               c_BinaryenAndInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenOrInt64"
               c_BinaryenOrInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenXorInt64"
               c_BinaryenXorInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenShlInt64"
               c_BinaryenShlInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenShrUInt64"
               c_BinaryenShrUInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenShrSInt64"
               c_BinaryenShrSInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenRotLInt64"
               c_BinaryenRotLInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenRotRInt64"
               c_BinaryenRotRInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenEqInt64"
               c_BinaryenEqInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenNeInt64"
               c_BinaryenNeInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenLtSInt64"
               c_BinaryenLtSInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenLtUInt64"
               c_BinaryenLtUInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenLeSInt64"
               c_BinaryenLeSInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenLeUInt64"
               c_BinaryenLeUInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenGtSInt64"
               c_BinaryenGtSInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenGtUInt64"
               c_BinaryenGtUInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenGeSInt64"
               c_BinaryenGeSInt64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenGeUInt64"
               c_BinaryenGeUInt64 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenAddFloat32" c_BinaryenAddFloat32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenSubFloat32" c_BinaryenSubFloat32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenMulFloat32" c_BinaryenMulFloat32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenDivFloat32" c_BinaryenDivFloat32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenCopySignFloat32" c_BinaryenCopySignFloat32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenMinFloat32" c_BinaryenMinFloat32 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenMaxFloat32" c_BinaryenMaxFloat32 ::
               BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenEqFloat32"
               c_BinaryenEqFloat32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenNeFloat32"
               c_BinaryenNeFloat32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenLtFloat32"
               c_BinaryenLtFloat32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenLeFloat32"
               c_BinaryenLeFloat32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenGtFloat32"
               c_BinaryenGtFloat32 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenGeFloat32"
               c_BinaryenGeFloat32 :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenAddFloat64" c_BinaryenAddFloat64 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenSubFloat64" c_BinaryenSubFloat64 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenMulFloat64" c_BinaryenMulFloat64 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenDivFloat64" c_BinaryenDivFloat64 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenCopySignFloat64" c_BinaryenCopySignFloat64 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenMinFloat64" c_BinaryenMinFloat64 ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenMaxFloat64" c_BinaryenMaxFloat64 ::
               BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenEqFloat64"
               c_BinaryenEqFloat64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenNeFloat64"
               c_BinaryenNeFloat64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenLtFloat64"
               c_BinaryenLtFloat64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenLeFloat64"
               c_BinaryenLeFloat64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenGtFloat64"
               c_BinaryenGtFloat64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenGeFloat64"
               c_BinaryenGeFloat64 :: BinaryenOp

foreign import ccall interruptible "binaryen-c.h BinaryenPageSize"
               c_BinaryenPageSize :: BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenCurrentMemory" c_BinaryenCurrentMemory ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenGrowMemory" c_BinaryenGrowMemory ::
               BinaryenOp

foreign import ccall interruptible
               "binaryen-c.h BinaryenHasFeature" c_BinaryenHasFeature ::
               BinaryenOp
