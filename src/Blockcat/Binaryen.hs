{-# LANGUAGE DuplicateRecordFields #-}

module Blockcat.Binaryen
    ( Index(..)
    , Type(..)
    , Op(..)
    , Expression(..)
    , FunctionType(..)
    , Function(..)
    , Import(..)
    , Export(..)
    , Module(..)
    ) where

import Data.ByteString
import Data.HashMap.Strict
import Data.Int
import Data.Vector
import Data.Word

newtype Index = Index
    { getIndex :: Word32
    }

data Type
    = None
    | I32
    | I64
    | F32
    | F64

data Op
    = ClzInt32
    | CtzInt32
    | PopcntInt32
    | NegFloat32
    | AbsFloat32
    | CeilFloat32
    | FloorFloat32
    | TruncFloat32
    | NearestFloat32
    | SqrtFloat32
    | EqZInt32
    | ClzInt64
    | CtzInt64
    | PopcntInt64
    | NegFloat64
    | AbsFloat64
    | CeilFloat64
    | FloorFloat64
    | TruncFloat64
    | NearestFloat64
    | SqrtFloat64
    | EqZInt64
    | ExtendSInt32
    | ExtendUInt32
    | WrapInt64
    | TruncSFloat32ToInt32
    | TruncSFloat32ToInt64
    | TruncUFloat32ToInt32
    | TruncUFloat32ToInt64
    | TruncSFloat64ToInt32
    | TruncSFloat64ToInt64
    | TruncUFloat64ToInt32
    | TruncUFloat64ToInt64
    | ReinterpretFloat32
    | ReinterpretFloat64
    | ConvertSInt32ToFloat32
    | ConvertSInt32ToFloat64
    | ConvertUInt32ToFloat32
    | ConvertUInt32ToFloat64
    | ConvertSInt64ToFloat32
    | ConvertSInt64ToFloat64
    | ConvertUInt64ToFloat32
    | ConvertUInt64ToFloat64
    | PromoteFloat32
    | DemoteFloat64
    | ReinterpretInt32
    | ReinterpretInt64
    | AddInt32
    | SubInt32
    | MulInt32
    | DivSInt32
    | DivUInt32
    | RemSInt32
    | RemUInt32
    | AndInt32
    | OrInt32
    | XorInt32
    | ShlInt32
    | ShrUInt32
    | ShrSInt32
    | RotLInt32
    | RotRInt32
    | EqInt32
    | NeInt32
    | LtSInt32
    | LtUInt32
    | LeSInt32
    | LeUInt32
    | GtSInt32
    | GtUInt32
    | GeSInt32
    | GeUInt32
    | AddInt64
    | SubInt64
    | MulInt64
    | DivSInt64
    | DivUInt64
    | RemSInt64
    | RemUInt64
    | AndInt64
    | OrInt64
    | XorInt64
    | ShlInt64
    | ShrUInt64
    | ShrSInt64
    | RotLInt64
    | RotRInt64
    | EqInt64
    | NeInt64
    | LtSInt64
    | LtUInt64
    | LeSInt64
    | LeUInt64
    | GtSInt64
    | GtUInt64
    | GeSInt64
    | GeUInt64
    | AddFloat32
    | SubFloat32
    | MulFloat32
    | DivFloat32
    | CopySignFloat32
    | MinFloat32
    | MaxFloat32
    | EqFloat32
    | NeFloat32
    | LtFloat32
    | LeFloat32
    | GtFloat32
    | GeFloat32
    | AddFloat64
    | SubFloat64
    | MulFloat64
    | DivFloat64
    | CopySignFloat64
    | MinFloat64
    | MaxFloat64
    | EqFloat64
    | NeFloat64
    | LtFloat64
    | LeFloat64
    | GtFloat64
    | GeFloat64
    | PageSize
    | CurrentMemory
    | GrowMemory
    | HasFeature

data Expression
    = Block { name :: ByteString
           ,  children :: Vector Expression}
    | If { condition, ifTrue, ifFalse :: Expression}
    | Loop { in_ :: ByteString
          ,  body :: Expression}
    | Break { name :: ByteString
           ,  condition, value :: Expression}
    | Switch { names :: Vector ByteString
            ,  defaultName :: ByteString
            ,  condition, value :: Expression}
    | Call { target :: ByteString
          ,  operands :: Vector Expression
          ,  returnType :: Type}
    | CallImport { target :: ByteString
                ,  operands :: Vector Expression
                ,  returnType :: Type}
    | CallIndirect { target_ :: Expression
                  ,  operands :: Vector Expression
                  ,  type__ :: ByteString}
    | GetLocal { index :: Index
              ,  type_ :: Type}
    | SetLocal { index :: Index
              ,  value :: Expression}
    | TeeLocal { index :: Index
              ,  value :: Expression}
    | Load { bytes :: Word32
          ,  signed :: Int8
          ,  offset, align :: Word32
          ,  type_ :: Type
          ,  ptr :: Expression}
    | Store { bytes, offset, align :: Word32
           ,  ptr, value :: Expression
           ,  type_ :: Type}
    | Unary { op :: Op
           ,  value :: Expression}
    | Binary { op :: Op
            ,  left, right :: Expression}
    | Select { condition, ifTrue, ifFalse :: Expression}
    | Drop { value :: Expression}
    | Return { value :: Expression}
    | Host { op :: Op
          ,  name :: ByteString
          ,  operands :: Vector Expression}
    | Nop
    | Unreachable

data FunctionType = FunctionType
    { name :: ByteString
    , result :: Type
    , paramTypes :: Vector Type
    }

data Function = Function
    { name :: ByteString
    , type_ :: FunctionType
    , varTypes :: Vector Type
    , body :: Expression
    }

data Import = Import
    { internalName, externalModuleName, externalBaseName :: ByteString
    , type_ :: FunctionType
    }

data Export = Export
    { internalName, externalName :: ByteString
    }

data Module = Module
    { functions :: HashMap ByteString Function
    , imports :: HashMap ByteString Import
    , exports :: HashMap ByteString Export
    }
