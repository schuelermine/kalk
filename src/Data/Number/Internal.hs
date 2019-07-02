{-# LANGUAGE DataKinds, TypeFamilies, GADTs, TypeOperators, NoStarIsType, KindSignatures, UndecidableInstances, PatternSynonyms #-}

module Data.Number.Internal where

import Data.Kind
import Data.Complex

data Natural :: Type where
    Zero :: Natural
    Succ :: Natural -> Natural

data Number :: Type where
    Number :: Integer -> Integer -> Natural -> Number

data Infinite :: Type -> Type where
    Infinite :: Infinite t
    Only :: t -> Infinite t

pattern Infinity = Infinite
pattern MinusInfinity = Infinite

data Error :: Type where
    DivDomain :: Error
    ArcsinDomain :: Error
    ArccosDomain :: Error
    ArcsecDomain :: Error
    ArccscDomain :: Error
    SeriesBoundsOrder :: Error
    Unsupported :: Error
    Other :: String -> Error

data Constant :: Type where
    Pi :: Constant
    Phi :: Constant
    E :: Constant
    I :: Constant

data Formula :: Natural -> Type where
    Error :: Error -> Formula n
    Unknown :: Natural -> Formula ('Succ n)
    Constant :: Constant -> Formula n
    Value :: Complex Number -> Formula n
    Addition :: Formula n -> Formula n -> Formula n
    Negation :: Formula n -> Formula n
    Multiplication :: Formula n -> Formula n -> Formula n
    Reciprocation :: Formula n -> Formula n
    Sum :: Infinite Natural -> Infinite Natural -> Formula ('Succ n) -> Formula n
    Product :: Infinite Natural -> Infinite Natural -> Formula ('Succ n) -> Formula n
    Integral :: Infinite Natural -> Infinite Natural -> Formula ('Succ n) -> Formula n
    Apply :: Natural -> Formula ('Succ n) -> Formula n
    Sine :: Formula n -> Formula n
    Cosine :: Formula n -> Formula n
    Tangent :: Formula n -> Formula n
    Cotangent :: Formula n -> Formula n
    Secant :: Formula n -> Formula n
    Cosecant :: Formula n -> Formula n