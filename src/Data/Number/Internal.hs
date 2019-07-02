{-# LANGUAGE DataKinds, TypeFamilies, GADTs, TypeOperators, NoStarIsType, KindSignatures, UndecidableInstances, PatternSynonyms #-}

module Data.Number.Internal where

import Data.Kind
import Data.Complex

data Natural :: Type where
    Zero :: Natural
    Succ :: Natural -> Natural

type One = Succ Zero
type Two = Succ One
type Three = Succ Two

data Number :: Type where
    Number :: Integer -> Integer -> Natural -> Number

data Infinite :: Type -> Type where
    -- Type with some kind of infinity,
    -- for example: ℝ+ > (-∞)
    Infinite :: Infinite t
    Only :: t -> Infinite t

pattern Infinity = Infinite
pattern MinusInfinity = Infinite

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

data Constant :: Domain -> Type where
    Pi :: Constant R
    Phi :: Constant R
    E :: Constant R
    I :: Constant C

data Domain :: Type where
    B :: Domain
    N :: Domain
    Z :: Domain
    Q :: Domain
    R :: Domain
    C :: Domain

data DomainHandling :: Natural -> Type where
    Union :: DomainHandling Two
    Simply :: Domain -> DomainHandling One
    Negate :: DomainHandling One

type family Union (a :: Domain) (b :: Domain) :: Domain
type instance Union N x = x
type instance Union x N = x
type instance Union Z Q = Q
type instance Union Z R = R
type instance Union Q Z = Q
type instance Union Q R = R
type instance Union R Z = R
type instance Union R Q = R
type instance Union C x = C
type instance Union x C = C

type family Negate (a :: Domain) :: Domain

type family HandleDomains (a :: DomainHandling Two) (b :: Domain) (c :: Domain) :: Domain
type instance HandleDomains 'Union

type family HandleDomain (a :: DomainHandling One) (b :: Domain) :: Domain

data Formula :: Specificity -> Domain -> Natural -> Type where
    Error :: Error -> Formula a b n
    Unknown :: Natural -> Formula a b ('Succ n)
    Constant :: Constant b -> Formula a b n
    RealValue :: Complex Number -> Formula a 'R n
    Unary :: Unary -> Formula a n -> Formula a n
    Binary :: Binary -> Formula a n -> Formula a n -> Formula a n
    Fold :: Infinite Integer -> Infinite Integer -> Formula Unspecific ('Succ ('Succ n)) -> Formula Unspecific ('Succ n) -> Formula a n
    Multiple :: [Formula a b n] -> Formula a b n
    -- for example: Fold (FromInteger '7) (FromInteger '10) (Operation Addition (Unknown one) (Unknown two)) (Operation Addition (Unknown one) (Unknown two))

data Operand :: DomainHandling Two -> Type where
    Addition :: Operand Two 'Union
    Multiplication :: Operand Two
    Exponentiation :: Operand Two
    Logarithm :: Operand Two
    Root :: Operand Two

data Unary :: DomainHandline One -> Type where
    Negation :: Unary One
    Reciprocation :: Unary One
    Sine :: Unary One
    Cosine :: Unary One
    Tangent :: Unary One
    Cotangent :: Unary One
    Secant :: Unary One
    Cosecant :: Unary One
    Arcsine :: Unary One
    Arccosine :: Unary One
    Arctangent :: Unary One
    Arccotangent :: Unary One
    Arcsecant :: Unary One
    Arccosecant :: Unary One