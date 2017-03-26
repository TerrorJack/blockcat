{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Blockcat.Binaryen.Marshal
    ( toBinaryenModuleRef
    ) where

import Blockcat.Binaryen.Raw
import Blockcat.Binaryen.Types
import Data.ByteString
import Data.Foldable
import Data.Functor
import Data.Vector as V
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

toBinaryenType :: Type -> BinaryenType
toBinaryenType None = c_BinaryenNone
toBinaryenType I32 = c_BinaryenInt32
toBinaryenType I64 = c_BinaryenInt64
toBinaryenType F32 = c_BinaryenFloat32
toBinaryenType F64 = c_BinaryenFloat64

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
                (fromIntegral $ V.length paramTypes)

--todo
addExpression :: BinaryenModuleRef -> Expression -> IO BinaryenExpressionRef
addExpression = undefined

addFunction :: BinaryenModuleRef -> Function -> IO BinaryenFunctionRef
addFunction m Function {..} =
    useAsCString name $ \n -> do
        ft <- addFunctionType m type_
        withStorablePtr (pure . toBinaryenType) varTypes $ \p -> do
            e <- addExpression m body
            c_BinaryenAddFunction m n ft p (fromIntegral $ V.length varTypes) e

--todo
addImport :: BinaryenModuleRef -> Import -> IO ()
addImport = undefined

addExport :: BinaryenModuleRef -> Export -> IO ()
addExport m Export {..} =
    void $
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
