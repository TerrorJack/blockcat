{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blockcat.Binaryen.Raw where

import Foreign
import Foreign.C

newtype BinaryenIndex = BinaryenIndex
    { getBinaryenIndex :: CUInt
    } deriving (Num)

newtype BinaryenType = BinaryenType
    { getBinaryenType :: CUInt
    } deriving (Storable)

foreign import ccall interruptible "BinaryenNone" c_BinaryenNone ::
               BinaryenType

foreign import ccall interruptible "BinaryenInt32" c_BinaryenInt32
               :: BinaryenType

foreign import ccall interruptible "BinaryenInt64" c_BinaryenInt64
               :: BinaryenType

foreign import ccall interruptible "BinaryenFloat32"
               c_BinaryenFloat32 :: BinaryenType

foreign import ccall interruptible "BinaryenFloat64"
               c_BinaryenFloat64 :: BinaryenType

newtype BinaryenModuleRef = BinaryenModuleRef
    { getBinaryenModuleRef :: Ptr ()
    }

foreign import ccall interruptible "BinaryenModuleCreate"
               c_BinaryenModuleCreate :: IO BinaryenModuleRef

foreign import ccall interruptible "BinaryenModuleDispose"
               c_BinaryenModuleDispose :: BinaryenModuleRef -> IO ()

newtype BinaryenFunctionTypeRef = BinaryenFunctionTypeRef
    { getBinaryenFunctionTypeRef :: Ptr ()
    }

foreign import ccall interruptible "BinaryenAddFunctionType"
               c_BinaryenAddFunctionType ::
               BinaryenModuleRef ->
                 CString ->
                   BinaryenType ->
                     Ptr BinaryenType -> BinaryenIndex -> IO BinaryenFunctionTypeRef

newtype BinaryenLiteralRef = BinaryenLiteralRef
    { getBinaryenLiteralRef :: Ptr ()
    }

foreign import ccall interruptible "BinaryenLiteralSize"
               c_BinaryenLiteralSize :: CSize

foreign import ccall interruptible "BinaryenLiteralAlign"
               c_BinaryenLiteralAlign :: CSize

foreign import ccall interruptible "BinaryenLiteralRefInt32"
               c_BinaryenLiteralRefInt32 :: BinaryenLiteralRef -> CInt -> IO ()

foreign import ccall interruptible "BinaryenLiteralRefInt64"
               c_BinaryenLiteralRefInt64 :: BinaryenLiteralRef -> CLLong -> IO ()

foreign import ccall interruptible "BinaryenLiteralRefFloat32"
               c_BinaryenLiteralRefFloat32 ::
               BinaryenLiteralRef -> CFloat -> IO ()

foreign import ccall interruptible "BinaryenLiteralRefFloat64"
               c_BinaryenLiteralRefFloat64 ::
               BinaryenLiteralRef -> CDouble -> IO ()

foreign import ccall interruptible "BinaryenLiteralRefFloat32Bits"
               c_BinaryenLiteralRefFloat32Bits ::
               BinaryenLiteralRef -> CInt -> IO ()

foreign import ccall interruptible "BinaryenLiteralRefFloat64Bits"
               c_BinaryenLiteralRefFloat64Bits ::
               BinaryenLiteralRef -> CLLong -> IO ()

newtype BinaryenOp = BinaryenOp
    { getBinaryenOp :: CInt
    }

foreign import ccall interruptible "BinaryenClzInt32"
               c_BinaryenClzInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenCtzInt32"
               c_BinaryenCtzInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenPopcntInt32"
               c_BinaryenPopcntInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenNegFloat32"
               c_BinaryenNegFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenAbsFloat32"
               c_BinaryenAbsFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenCeilFloat32"
               c_BinaryenCeilFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenFloorFloat32"
               c_BinaryenFloorFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenTruncFloat32"
               c_BinaryenTruncFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenNearestFloat32"
               c_BinaryenNearestFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenSqrtFloat32"
               c_BinaryenSqrtFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenEqZInt32"
               c_BinaryenEqZInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenClzInt64"
               c_BinaryenClzInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenCtzInt64"
               c_BinaryenCtzInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenPopcntInt64"
               c_BinaryenPopcntInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenNegFloat64"
               c_BinaryenNegFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenAbsFloat64"
               c_BinaryenAbsFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenCeilFloat64"
               c_BinaryenCeilFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenFloorFloat64"
               c_BinaryenFloorFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenTruncFloat64"
               c_BinaryenTruncFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenNearestFloat64"
               c_BinaryenNearestFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenSqrtFloat64"
               c_BinaryenSqrtFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenEqZInt64"
               c_BinaryenEqZInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenExtendSInt32"
               c_BinaryenExtendSInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenExtendUInt32"
               c_BinaryenExtendUInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenWrapInt64"
               c_BinaryenWrapInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenTruncSFloat32ToInt32"
               c_BinaryenTruncSFloat32ToInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenTruncSFloat32ToInt64"
               c_BinaryenTruncSFloat32ToInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenTruncUFloat32ToInt32"
               c_BinaryenTruncUFloat32ToInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenTruncUFloat32ToInt64"
               c_BinaryenTruncUFloat32ToInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenTruncSFloat64ToInt32"
               c_BinaryenTruncSFloat64ToInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenTruncSFloat64ToInt64"
               c_BinaryenTruncSFloat64ToInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenTruncUFloat64ToInt32"
               c_BinaryenTruncUFloat64ToInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenTruncUFloat64ToInt64"
               c_BinaryenTruncUFloat64ToInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenReinterpretFloat32"
               c_BinaryenReinterpretFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenReinterpretFloat64"
               c_BinaryenReinterpretFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenConvertSInt32ToFloat32"
               c_BinaryenConvertSInt32ToFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenConvertSInt32ToFloat64"
               c_BinaryenConvertSInt32ToFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenConvertUInt32ToFloat32"
               c_BinaryenConvertUInt32ToFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenConvertUInt32ToFloat64"
               c_BinaryenConvertUInt32ToFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenConvertSInt64ToFloat32"
               c_BinaryenConvertSInt64ToFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenConvertSInt64ToFloat64"
               c_BinaryenConvertSInt64ToFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenConvertUInt64ToFloat32"
               c_BinaryenConvertUInt64ToFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenConvertUInt64ToFloat64"
               c_BinaryenConvertUInt64ToFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenPromoteFloat32"
               c_BinaryenPromoteFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenDemoteFloat64"
               c_BinaryenDemoteFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenReinterpretInt32"
               c_BinaryenReinterpretInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenReinterpretInt64"
               c_BinaryenReinterpretInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenAddInt32"
               c_BinaryenAddInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenSubInt32"
               c_BinaryenSubInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenMulInt32"
               c_BinaryenMulInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenDivSInt32"
               c_BinaryenDivSInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenDivUInt32"
               c_BinaryenDivUInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenRemSInt32"
               c_BinaryenRemSInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenRemUInt32"
               c_BinaryenRemUInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenAndInt32"
               c_BinaryenAndInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenOrInt32"
               c_BinaryenOrInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenXorInt32"
               c_BinaryenXorInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenShlInt32"
               c_BinaryenShlInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenShrUInt32"
               c_BinaryenShrUInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenShrSInt32"
               c_BinaryenShrSInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenRotLInt32"
               c_BinaryenRotLInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenRotRInt32"
               c_BinaryenRotRInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenEqInt32"
               c_BinaryenEqInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenNeInt32"
               c_BinaryenNeInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenLtSInt32"
               c_BinaryenLtSInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenLtUInt32"
               c_BinaryenLtUInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenLeSInt32"
               c_BinaryenLeSInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenLeUInt32"
               c_BinaryenLeUInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenGtSInt32"
               c_BinaryenGtSInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenGtUInt32"
               c_BinaryenGtUInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenGeSInt32"
               c_BinaryenGeSInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenGeUInt32"
               c_BinaryenGeUInt32 :: BinaryenOp

foreign import ccall interruptible "BinaryenAddInt64"
               c_BinaryenAddInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenSubInt64"
               c_BinaryenSubInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenMulInt64"
               c_BinaryenMulInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenDivSInt64"
               c_BinaryenDivSInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenDivUInt64"
               c_BinaryenDivUInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenRemSInt64"
               c_BinaryenRemSInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenRemUInt64"
               c_BinaryenRemUInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenAndInt64"
               c_BinaryenAndInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenOrInt64"
               c_BinaryenOrInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenXorInt64"
               c_BinaryenXorInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenShlInt64"
               c_BinaryenShlInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenShrUInt64"
               c_BinaryenShrUInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenShrSInt64"
               c_BinaryenShrSInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenRotLInt64"
               c_BinaryenRotLInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenRotRInt64"
               c_BinaryenRotRInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenEqInt64"
               c_BinaryenEqInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenNeInt64"
               c_BinaryenNeInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenLtSInt64"
               c_BinaryenLtSInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenLtUInt64"
               c_BinaryenLtUInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenLeSInt64"
               c_BinaryenLeSInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenLeUInt64"
               c_BinaryenLeUInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenGtSInt64"
               c_BinaryenGtSInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenGtUInt64"
               c_BinaryenGtUInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenGeSInt64"
               c_BinaryenGeSInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenGeUInt64"
               c_BinaryenGeUInt64 :: BinaryenOp

foreign import ccall interruptible "BinaryenAddFloat32"
               c_BinaryenAddFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenSubFloat32"
               c_BinaryenSubFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenMulFloat32"
               c_BinaryenMulFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenDivFloat32"
               c_BinaryenDivFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenCopySignFloat32"
               c_BinaryenCopySignFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenMinFloat32"
               c_BinaryenMinFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenMaxFloat32"
               c_BinaryenMaxFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenEqFloat32"
               c_BinaryenEqFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenNeFloat32"
               c_BinaryenNeFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenLtFloat32"
               c_BinaryenLtFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenLeFloat32"
               c_BinaryenLeFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenGtFloat32"
               c_BinaryenGtFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenGeFloat32"
               c_BinaryenGeFloat32 :: BinaryenOp

foreign import ccall interruptible "BinaryenAddFloat64"
               c_BinaryenAddFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenSubFloat64"
               c_BinaryenSubFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenMulFloat64"
               c_BinaryenMulFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenDivFloat64"
               c_BinaryenDivFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenCopySignFloat64"
               c_BinaryenCopySignFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenMinFloat64"
               c_BinaryenMinFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenMaxFloat64"
               c_BinaryenMaxFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenEqFloat64"
               c_BinaryenEqFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenNeFloat64"
               c_BinaryenNeFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenLtFloat64"
               c_BinaryenLtFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenLeFloat64"
               c_BinaryenLeFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenGtFloat64"
               c_BinaryenGtFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenGeFloat64"
               c_BinaryenGeFloat64 :: BinaryenOp

foreign import ccall interruptible "BinaryenPageSize"
               c_BinaryenPageSize :: BinaryenOp

foreign import ccall interruptible "BinaryenCurrentMemory"
               c_BinaryenCurrentMemory :: BinaryenOp

foreign import ccall interruptible "BinaryenGrowMemory"
               c_BinaryenGrowMemory :: BinaryenOp

foreign import ccall interruptible "BinaryenHasFeature"
               c_BinaryenHasFeature :: BinaryenOp

newtype BinaryenExpressionRef = BinaryenExpressionRef
    { getBinaryenExpressionRef :: Ptr ()
    } deriving (Storable)

foreign import ccall interruptible "BinaryenBlock" c_BinaryenBlock
               ::
               BinaryenModuleRef ->
                 CString ->
                   Ptr BinaryenExpressionRef ->
                     BinaryenIndex -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenIf" c_BinaryenIf ::
               BinaryenModuleRef ->
                 BinaryenExpressionRef ->
                   BinaryenExpressionRef ->
                     BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenLoop" c_BinaryenLoop ::
               BinaryenModuleRef ->
                 CString -> BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenBreak" c_BinaryenBreak
               ::
               BinaryenModuleRef ->
                 CString ->
                   BinaryenExpressionRef ->
                     BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenSwitch"
               c_BinaryenSwitch ::
               BinaryenModuleRef ->
                 Ptr CString ->
                   BinaryenIndex ->
                     CString ->
                       BinaryenExpressionRef ->
                         BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenCall" c_BinaryenCall ::
               BinaryenModuleRef ->
                 CString ->
                   Ptr BinaryenExpressionRef ->
                     BinaryenIndex -> BinaryenType -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenCallImport"
               c_BinaryenCallImport ::
               BinaryenModuleRef ->
                 CString ->
                   Ptr BinaryenExpressionRef ->
                     BinaryenIndex -> BinaryenType -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenCallIndirect"
               c_BinaryenCallIndirect ::
               BinaryenModuleRef ->
                 BinaryenExpressionRef ->
                   Ptr BinaryenExpressionRef ->
                     BinaryenIndex -> CString -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenGetLocal"
               c_BinaryenGetLocal ::
               BinaryenModuleRef ->
                 BinaryenIndex -> BinaryenType -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenSetLocal"
               c_BinaryenSetLocal ::
               BinaryenModuleRef ->
                 BinaryenIndex -> BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenTeeLocal"
               c_BinaryenTeeLocal ::
               BinaryenModuleRef ->
                 BinaryenIndex -> BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenLoad" c_BinaryenLoad ::
               BinaryenModuleRef ->
                 CUInt ->
                   CChar ->
                     CUInt ->
                       CUInt ->
                         BinaryenType -> BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenStore" c_BinaryenStore
               ::
               BinaryenModuleRef ->
                 CUInt ->
                   CUInt ->
                     CUInt ->
                       BinaryenExpressionRef ->
                         BinaryenExpressionRef -> BinaryenType -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenConstRef"
               c_BinaryenConstRef ::
               BinaryenModuleRef -> BinaryenLiteralRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenUnary" c_BinaryenUnary
               ::
               BinaryenModuleRef ->
                 BinaryenOp -> BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenBinary"
               c_BinaryenBinary ::
               BinaryenModuleRef ->
                 BinaryenOp ->
                   BinaryenExpressionRef ->
                     BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenSelect"
               c_BinaryenSelect ::
               BinaryenModuleRef ->
                 BinaryenExpressionRef ->
                   BinaryenExpressionRef ->
                     BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenDrop" c_BinaryenDrop ::
               BinaryenModuleRef ->
                 BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenReturn"
               c_BinaryenReturn ::
               BinaryenModuleRef ->
                 BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenHost" c_BinaryenHost ::
               BinaryenModuleRef ->
                 BinaryenOp ->
                   CString ->
                     Ptr BinaryenExpressionRef ->
                       BinaryenIndex -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenNop" c_BinaryenNop ::
               BinaryenModuleRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenUnreachable"
               c_BinaryenUnreachable ::
               BinaryenModuleRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenExpressionPrint"
               c_BinaryenExpressionPrint :: BinaryenExpressionRef -> IO ()

newtype BinaryenFunctionRef = BinaryenFunctionRef
    { getBinaryenFunctionRef :: Ptr ()
    }

foreign import ccall interruptible "BinaryenAddFunction"
               c_BinaryenAddFunction ::
               BinaryenModuleRef ->
                 CString ->
                   BinaryenFunctionTypeRef ->
                     Ptr BinaryenType ->
                       BinaryenIndex -> BinaryenExpressionRef -> IO BinaryenFunctionRef

newtype BinaryenImportRef = BinaryenImportRef
    { getBinaryenImportRef :: Ptr ()
    }

foreign import ccall interruptible "BinaryenAddImport"
               c_BinaryenAddImport ::
               BinaryenModuleRef ->
                 CString ->
                   CString ->
                     CString -> BinaryenFunctionTypeRef -> IO BinaryenImportRef

newtype BinaryenExportRef = BinaryenExportRef
    { getBinaryenExportRef :: Ptr ()
    }

foreign import ccall interruptible "BinaryenAddExport"
               c_BinaryenAddExport ::
               BinaryenModuleRef -> CString -> CString -> IO BinaryenExportRef

foreign import ccall interruptible "BinaryenSetFunctionTable"
               c_BinaryenSetFunctionTable ::
               BinaryenModuleRef ->
                 Ptr BinaryenFunctionRef -> BinaryenIndex -> IO ()

foreign import ccall interruptible "BinaryenSetMemory"
               c_BinaryenSetMemory ::
               BinaryenModuleRef ->
                 BinaryenIndex ->
                   BinaryenIndex ->
                     CString ->
                       Ptr CString ->
                         Ptr BinaryenExpressionRef ->
                           Ptr BinaryenIndex -> BinaryenIndex -> IO ()

foreign import ccall interruptible "BinaryenSetStart"
               c_BinaryenSetStart ::
               BinaryenModuleRef -> BinaryenFunctionRef -> IO ()

foreign import ccall interruptible "BinaryenModulePrint"
               c_BinaryenModulePrint :: BinaryenModuleRef -> IO ()

foreign import ccall interruptible "BinaryenModuleValidate"
               c_BinaryenModuleValidate :: BinaryenModuleRef -> IO CInt

foreign import ccall interruptible "BinaryenModuleOptimize"
               c_BinaryenModuleOptimize :: BinaryenModuleRef -> IO ()

foreign import ccall interruptible "BinaryenModuleAutoDrop"
               c_BinaryenModuleAutoDrop :: BinaryenModuleRef -> IO ()

foreign import ccall interruptible "BinaryenModuleWrite"
               c_BinaryenModuleWrite ::
               BinaryenModuleRef -> CString -> CSize -> IO CSize

foreign import ccall interruptible "BinaryenModuleRead"
               c_BinaryenModuleRead :: CString -> CSize -> IO BinaryenModuleRef

foreign import ccall interruptible "BinaryenModuleInterpret"
               c_BinaryenModuleInterpret :: BinaryenModuleRef -> IO ()

newtype RelooperRef = RelooperRef
    { getRelooperRef :: Ptr ()
    }

newtype RelooperBlockRef = RelooperBlockRef
    { getRelooperBlockRef :: Ptr ()
    }

foreign import ccall interruptible "RelooperCreate"
               c_RelooperCreate :: IO RelooperRef

foreign import ccall interruptible "RelooperAddBlock"
               c_RelooperAddBlock ::
               RelooperRef -> BinaryenExpressionRef -> IO RelooperBlockRef

foreign import ccall interruptible "RelooperAddBranch"
               c_RelooperAddBranch ::
               RelooperBlockRef ->
                 RelooperBlockRef ->
                   BinaryenExpressionRef -> BinaryenExpressionRef -> IO ()

foreign import ccall interruptible "RelooperAddBlockWithSwitch"
               c_RelooperAddBlockWithSwitch ::
               RelooperRef ->
                 BinaryenExpressionRef ->
                   BinaryenExpressionRef -> IO RelooperBlockRef

foreign import ccall interruptible "RelooperAddBranchForSwitch"
               c_RelooperAddBranchForSwitch ::
               RelooperBlockRef ->
                 RelooperBlockRef ->
                   Ptr BinaryenIndex ->
                     BinaryenIndex -> BinaryenExpressionRef -> IO ()

foreign import ccall interruptible "RelooperRenderAndDispose"
               c_RelooperRenderAndDispose ::
               RelooperRef ->
                 RelooperBlockRef ->
                   BinaryenIndex -> BinaryenModuleRef -> IO BinaryenExpressionRef

foreign import ccall interruptible "BinaryenSetAPITracing"
               c_BinaryenSetAPITracing :: CInt -> IO ()
