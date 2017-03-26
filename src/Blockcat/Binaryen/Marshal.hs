{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Blockcat.Binaryen.Marshal
    ( toBinaryenModuleRef
    ) where

import Blockcat.Binaryen.Raw
import Blockcat.Binaryen.Types
import Data.ByteString (useAsCString)
import Data.Coerce
import Data.Foldable
import Data.Vector as V
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

lengthBinaryenIndex :: Vector t -> BinaryenIndex
lengthBinaryenIndex = fromIntegral . V.length

toBinaryenType :: Type -> BinaryenType
toBinaryenType None = c_BinaryenNone
toBinaryenType I32 = c_BinaryenInt32
toBinaryenType I64 = c_BinaryenInt64
toBinaryenType F32 = c_BinaryenFloat32
toBinaryenType F64 = c_BinaryenFloat64

toBinaryenOp :: Op -> BinaryenOp
toBinaryenOp op =
    case op of
        ClzInt32 -> c_BinaryenClzInt32
        CtzInt32 -> c_BinaryenCtzInt32
        PopcntInt32 -> c_BinaryenPopcntInt32
        NegFloat32 -> c_BinaryenNegFloat32
        AbsFloat32 -> c_BinaryenAbsFloat32
        CeilFloat32 -> c_BinaryenCeilFloat32
        FloorFloat32 -> c_BinaryenFloorFloat32
        TruncFloat32 -> c_BinaryenTruncFloat32
        NearestFloat32 -> c_BinaryenNearestFloat32
        SqrtFloat32 -> c_BinaryenSqrtFloat32
        EqZInt32 -> c_BinaryenEqZInt32
        ClzInt64 -> c_BinaryenClzInt64
        CtzInt64 -> c_BinaryenCtzInt64
        PopcntInt64 -> c_BinaryenPopcntInt64
        NegFloat64 -> c_BinaryenNegFloat64
        AbsFloat64 -> c_BinaryenAbsFloat64
        CeilFloat64 -> c_BinaryenCeilFloat64
        FloorFloat64 -> c_BinaryenFloorFloat64
        TruncFloat64 -> c_BinaryenTruncFloat64
        NearestFloat64 -> c_BinaryenNearestFloat64
        SqrtFloat64 -> c_BinaryenSqrtFloat64
        EqZInt64 -> c_BinaryenEqZInt64
        ExtendSInt32 -> c_BinaryenExtendSInt32
        ExtendUInt32 -> c_BinaryenExtendUInt32
        WrapInt64 -> c_BinaryenWrapInt64
        TruncSFloat32ToInt32 -> c_BinaryenTruncSFloat32ToInt32
        TruncSFloat32ToInt64 -> c_BinaryenTruncSFloat32ToInt64
        TruncUFloat32ToInt32 -> c_BinaryenTruncUFloat32ToInt32
        TruncUFloat32ToInt64 -> c_BinaryenTruncUFloat32ToInt64
        TruncSFloat64ToInt32 -> c_BinaryenTruncSFloat64ToInt32
        TruncSFloat64ToInt64 -> c_BinaryenTruncSFloat64ToInt64
        TruncUFloat64ToInt32 -> c_BinaryenTruncUFloat64ToInt32
        TruncUFloat64ToInt64 -> c_BinaryenTruncUFloat64ToInt64
        ReinterpretFloat32 -> c_BinaryenReinterpretFloat32
        ReinterpretFloat64 -> c_BinaryenReinterpretFloat64
        ConvertSInt32ToFloat32 -> c_BinaryenConvertSInt32ToFloat32
        ConvertSInt32ToFloat64 -> c_BinaryenConvertSInt32ToFloat64
        ConvertUInt32ToFloat32 -> c_BinaryenConvertUInt32ToFloat32
        ConvertUInt32ToFloat64 -> c_BinaryenConvertUInt32ToFloat64
        ConvertSInt64ToFloat32 -> c_BinaryenConvertSInt64ToFloat32
        ConvertSInt64ToFloat64 -> c_BinaryenConvertSInt64ToFloat64
        ConvertUInt64ToFloat32 -> c_BinaryenConvertUInt64ToFloat32
        ConvertUInt64ToFloat64 -> c_BinaryenConvertUInt64ToFloat64
        PromoteFloat32 -> c_BinaryenPromoteFloat32
        DemoteFloat64 -> c_BinaryenDemoteFloat64
        ReinterpretInt32 -> c_BinaryenReinterpretInt32
        ReinterpretInt64 -> c_BinaryenReinterpretInt64
        AddInt32 -> c_BinaryenAddInt32
        SubInt32 -> c_BinaryenSubInt32
        MulInt32 -> c_BinaryenMulInt32
        DivSInt32 -> c_BinaryenDivSInt32
        DivUInt32 -> c_BinaryenDivUInt32
        RemSInt32 -> c_BinaryenRemSInt32
        RemUInt32 -> c_BinaryenRemUInt32
        AndInt32 -> c_BinaryenAndInt32
        OrInt32 -> c_BinaryenOrInt32
        XorInt32 -> c_BinaryenXorInt32
        ShlInt32 -> c_BinaryenShlInt32
        ShrUInt32 -> c_BinaryenShrUInt32
        ShrSInt32 -> c_BinaryenShrSInt32
        RotLInt32 -> c_BinaryenRotLInt32
        RotRInt32 -> c_BinaryenRotRInt32
        EqInt32 -> c_BinaryenEqInt32
        NeInt32 -> c_BinaryenNeInt32
        LtSInt32 -> c_BinaryenLtSInt32
        LtUInt32 -> c_BinaryenLtUInt32
        LeSInt32 -> c_BinaryenLeSInt32
        LeUInt32 -> c_BinaryenLeUInt32
        GtSInt32 -> c_BinaryenGtSInt32
        GtUInt32 -> c_BinaryenGtUInt32
        GeSInt32 -> c_BinaryenGeSInt32
        GeUInt32 -> c_BinaryenGeUInt32
        AddInt64 -> c_BinaryenAddInt64
        SubInt64 -> c_BinaryenSubInt64
        MulInt64 -> c_BinaryenMulInt64
        DivSInt64 -> c_BinaryenDivSInt64
        DivUInt64 -> c_BinaryenDivUInt64
        RemSInt64 -> c_BinaryenRemSInt64
        RemUInt64 -> c_BinaryenRemUInt64
        AndInt64 -> c_BinaryenAndInt64
        OrInt64 -> c_BinaryenOrInt64
        XorInt64 -> c_BinaryenXorInt64
        ShlInt64 -> c_BinaryenShlInt64
        ShrUInt64 -> c_BinaryenShrUInt64
        ShrSInt64 -> c_BinaryenShrSInt64
        RotLInt64 -> c_BinaryenRotLInt64
        RotRInt64 -> c_BinaryenRotRInt64
        EqInt64 -> c_BinaryenEqInt64
        NeInt64 -> c_BinaryenNeInt64
        LtSInt64 -> c_BinaryenLtSInt64
        LtUInt64 -> c_BinaryenLtUInt64
        LeSInt64 -> c_BinaryenLeSInt64
        LeUInt64 -> c_BinaryenLeUInt64
        GtSInt64 -> c_BinaryenGtSInt64
        GtUInt64 -> c_BinaryenGtUInt64
        GeSInt64 -> c_BinaryenGeSInt64
        GeUInt64 -> c_BinaryenGeUInt64
        AddFloat32 -> c_BinaryenAddFloat32
        SubFloat32 -> c_BinaryenSubFloat32
        MulFloat32 -> c_BinaryenMulFloat32
        DivFloat32 -> c_BinaryenDivFloat32
        CopySignFloat32 -> c_BinaryenCopySignFloat32
        MinFloat32 -> c_BinaryenMinFloat32
        MaxFloat32 -> c_BinaryenMaxFloat32
        EqFloat32 -> c_BinaryenEqFloat32
        NeFloat32 -> c_BinaryenNeFloat32
        LtFloat32 -> c_BinaryenLtFloat32
        LeFloat32 -> c_BinaryenLeFloat32
        GtFloat32 -> c_BinaryenGtFloat32
        GeFloat32 -> c_BinaryenGeFloat32
        AddFloat64 -> c_BinaryenAddFloat64
        SubFloat64 -> c_BinaryenSubFloat64
        MulFloat64 -> c_BinaryenMulFloat64
        DivFloat64 -> c_BinaryenDivFloat64
        CopySignFloat64 -> c_BinaryenCopySignFloat64
        MinFloat64 -> c_BinaryenMinFloat64
        MaxFloat64 -> c_BinaryenMaxFloat64
        EqFloat64 -> c_BinaryenEqFloat64
        NeFloat64 -> c_BinaryenNeFloat64
        LtFloat64 -> c_BinaryenLtFloat64
        LeFloat64 -> c_BinaryenLeFloat64
        GtFloat64 -> c_BinaryenGtFloat64
        GeFloat64 -> c_BinaryenGeFloat64
        PageSize -> c_BinaryenPageSize
        CurrentMemory -> c_BinaryenCurrentMemory
        GrowMemory -> c_BinaryenGrowMemory
        HasFeature -> c_BinaryenHasFeature

withStorablePtr
    :: Storable e
    => (t -> IO e) -> Vector t -> (Ptr e -> IO a) -> IO a
withStorablePtr t v f =
    allocaBytesAligned (V.length v * sizeOf (rogue t)) (alignment (rogue t)) $ \p -> do
        flip V.imapM_ v $ \i a -> do
            x <- t a
            pokeElemOff p i x
        f p
  where
    rogue :: (t -> IO e) -> e
    rogue = undefined

withScopedStorablePtrs :: (t -> (Ptr e -> IO a) -> IO a)
                       -> Vector t
                       -> (Ptr (Ptr e) -> IO a)
                       -> IO a
withScopedStorablePtrs c v f =
    allocaBytesAligned
        (V.length v * sizeOf (undefined :: Ptr ()))
        (alignment (undefined :: Ptr ())) $ \p ->
        let w l =
                if V.null l
                    then f p
                    else c (V.head l) $ \p' -> do
                             pokeElemOff p (V.length v - V.length l) p'
                             w $ V.tail l
        in w v

addFunctionType :: BinaryenModuleRef
                -> FunctionType
                -> IO BinaryenFunctionTypeRef
addFunctionType m FunctionType {..} =
    useAsCString name $ \n ->
        withStorablePtr (pure . toBinaryenType) paramTypes $ \p ->
            c_BinaryenAddFunctionType
                m
                n
                (toBinaryenType result)
                p
                (lengthBinaryenIndex paramTypes)

addExpression :: BinaryenModuleRef -> Expression -> IO BinaryenExpressionRef
addExpression m e =
    case e of
        Block {..} ->
            useAsCString name $ \n ->
                withStorablePtr (addExpression m) children $ \p ->
                    c_BinaryenBlock m n p $ lengthBinaryenIndex children
        If {..} -> do
            c <- addExpression m condition
            t <- addExpression m ifTrue
            f <- addExpression m ifFalse
            c_BinaryenIf m c t f
        Loop {..} ->
            useAsCString in_ $ \i -> do
                b <- addExpression m body
                c_BinaryenLoop m i b
        Break {..} ->
            useAsCString name $ \n -> do
                c <- addExpression m condition
                v <- addExpression m value
                c_BinaryenBreak m n c v
        Switch {..} ->
            withScopedStorablePtrs useAsCString names $ \ns ->
                useAsCString defaultName $ \dn -> do
                    c <- addExpression m condition
                    v <- addExpression m value
                    c_BinaryenSwitch m ns (lengthBinaryenIndex names) dn c v
        Call {..} ->
            useAsCString target $ \t ->
                withStorablePtr (addExpression m) operands $ \p ->
                    c_BinaryenCall
                        m
                        t
                        p
                        (lengthBinaryenIndex operands)
                        (toBinaryenType returnType)
        CallImport {..} ->
            useAsCString target $ \t ->
                withStorablePtr (addExpression m) operands $ \p ->
                    c_BinaryenCallImport
                        m
                        t
                        p
                        (lengthBinaryenIndex operands)
                        (toBinaryenType returnType)
        CallIndirect {..} -> do
            t <- addExpression m target_
            withStorablePtr (addExpression m) operands $ \p ->
                useAsCString type__ $ \t' ->
                    c_BinaryenCallIndirect
                        m
                        t
                        p
                        (lengthBinaryenIndex operands)
                        t'
        GetLocal {..} ->
            c_BinaryenGetLocal m (coerce index) (toBinaryenType type_)
        SetLocal {..} -> do
            v <- addExpression m value
            c_BinaryenSetLocal m (coerce index) v
        TeeLocal {..} -> do
            v <- addExpression m value
            c_BinaryenTeeLocal m (coerce index) v
        Load {..} -> do
            p <- addExpression m ptr
            c_BinaryenLoad
                m
                (coerce bytes)
                (coerce signed)
                (coerce offset)
                (coerce align)
                (toBinaryenType type_)
                p
        Store {..} -> do
            p <- addExpression m ptr
            v <- addExpression m value
            c_BinaryenStore
                m
                (coerce bytes)
                (coerce offset)
                (coerce align)
                p
                v
                (toBinaryenType type_)
        Const lit ->
            allocaBytesAligned
                (fromIntegral c_BinaryenLiteralSize)
                (fromIntegral c_BinaryenLiteralAlign) $ \p -> do
                case lit of
                    I32Literal x ->
                        c_BinaryenLiteralRefInt32 (coerce p) (coerce x)
                    I64Literal x ->
                        c_BinaryenLiteralRefInt64 (coerce p) (coerce x)
                    F32Literal x ->
                        c_BinaryenLiteralRefFloat32 (coerce p) (coerce x)
                    F64Literal x ->
                        c_BinaryenLiteralRefFloat64 (coerce p) (coerce x)
                c_BinaryenConstRef m (coerce p)
        Unary {..} -> do
            v <- addExpression m value
            c_BinaryenUnary m (toBinaryenOp op) v
        Binary {..} -> do
            l <- addExpression m left
            r <- addExpression m right
            c_BinaryenBinary m (toBinaryenOp op) l r
        Select {..} -> do
            c <- addExpression m condition
            t <- addExpression m ifTrue
            f <- addExpression m ifFalse
            c_BinaryenSelect m c t f
        Drop {..} -> do
            v <- addExpression m value
            c_BinaryenDrop m v
        Return {..} -> do
            v <- addExpression m value
            c_BinaryenReturn m v
        Host {..} ->
            useAsCString name $ \n ->
                withStorablePtr (addExpression m) operands $ \p ->
                    c_BinaryenHost
                        m
                        (toBinaryenOp op)
                        n
                        p
                        (lengthBinaryenIndex operands)
        Nop -> c_BinaryenNop m
        Unreachable -> c_BinaryenUnreachable m

addFunction :: BinaryenModuleRef -> Function -> IO BinaryenFunctionRef
addFunction m Function {..} =
    useAsCString name $ \n -> do
        ft <- addFunctionType m type_
        withStorablePtr (pure . toBinaryenType) varTypes $ \p -> do
            e <- addExpression m body
            c_BinaryenAddFunction m n ft p (lengthBinaryenIndex varTypes) e

addImport :: BinaryenModuleRef -> Import -> IO BinaryenImportRef
addImport m Import {..} =
    useAsCString internalName $ \i ->
        useAsCString externalModuleName $ \emn ->
            useAsCString externalBaseName $ \ebn -> do
                t <- addFunctionType m type_
                c_BinaryenAddImport m i emn ebn t

addExport :: BinaryenModuleRef -> Export -> IO BinaryenExportRef
addExport m Export {..} =
    useAsCString internalName $ \i ->
        useAsCString externalName $ \e -> c_BinaryenAddExport m i e

--todo
toBinaryenModuleRef :: Module -> IO BinaryenModuleRef
toBinaryenModuleRef Module {..} = do
    m <- c_BinaryenModuleCreate
    for_ functions $ addFunction m
    for_ imports $ addImport m
    for_ exports $ addExport m
    pure m
