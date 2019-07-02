{-# LANGUAGE DataKinds, TypeFamilies, GADTs, TypeOperators, NoStarIsType, KindSignatures, UndecidableInstances, PatternSynonyms #-}

module Data.Number.Types where

import Data.Kind
import Data.Complex

data Natural :: Type where
    Zero :: Natural
    Succ :: Natural -> Natural

type One = 'Succ 'Zero
type Two = 'Succ One
type Three = 'Succ Two

data Number :: Type where
    Number :: Integer -> Integer -> Natural -> Number

data Infinite :: Type -> Type where
    -- Type with some kind of infinity,
    -- for example: ℝ+\{-∞} or ℝ̂
    Infinite :: Infinite t
    Only :: t -> Infinite t

pattern Infinity = Infinite
pattern Infinity :: Infinite a
pattern MinusInfinity = Infinite
pattern MinusInfinity :: Infinite a

type NegativlyInfinite = Infinite

data Error :: Type where
    OutOfDomain :: Operand n -> Error
    -- for example: arcsin 2, 1/0
    -- arcsin 2 produces OutOfDomain Arcsine
    -- 1/0 produces OutOfDomain Reciprocation
    UnsupportedFunctionality :: Error
    Other :: String -> Error
    -- for partial functions on Formulas, such as integrate

data Specificity :: Type where
    Specific :: Specificity
    Unspecific :: Specificity

data Constant :: Type where
    Pi :: Constant
    Phi :: Constant
    E :: Constant
    I :: Constant

data Domain :: Type where
    N :: Domain
    Z :: Domain
    Q :: Domain
    R :: Domain
    C :: Domain

data Formula :: Type -> Natural -> Type where
    Error :: Error -> Formula a n
    Unknown :: a -> Formula a ('Succ n)
    Constant :: Constant -> Formula a n
    Value :: Complex Number -> Formula a n
    Function :: Operand One -> Formula a n -> Formula a n
    Operation :: Operand Two -> Formula a n -> Formula a n -> Formula a n
    Fold :: Infinite Integer -> NegativlyInfinite Integer -> Formula b Two -> Formula a ('Succ n) -> Formula a n
    -- for example: Fold (FromInteger '7) (FromInteger '10) (Operation Addition (Unknown one) (Unknown two)) (Operation Addition (Unknown one) (Unknown two))

data Operand :: Natural -> Type where
    Addition :: Operand Two
    Negation :: Operand One
    Multiplication :: Operand Two
    Reciprocation :: Operand One
    Sine :: Operand One
    Cosine :: Operand One
    Tangent :: Operand One
    Cotangent :: Operand One
    Secant :: Operand One
    Cosecant :: Operand One
    Arcsine :: Operand One
    Arccosine :: Operand One
    Arctangent :: Operand One
    Arccotangent :: Operand One
    Arcsecant :: Operand One
    Arccosecant :: Operand One
    Exponentiation :: Operand Two
    Logarithm :: Operand Two
    Root :: Operand Two