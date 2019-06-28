module Data.Number.Internal where

import Data.Scientific
import Data.Complex
import Numeric.Natural

data N  = N (Complex Scientific)
        | V C
        | NaN
        | Sum {summand1 :: N, summand2 :: N}
        | Negative {unnegate :: N}
        | Product {factor1 :: N, factor2 :: N}
        | Reciprocal {unreciprocal :: N}
        | Difference {minuend :: N, subtrahend :: N}
        | Quotient {dividend :: N, divisor :: N}
        | Root {index :: N, radicand :: N}
        | Power {base :: N, exponent :: N}
        | Logarithm {logbase :: N, logval :: N}
        | Sine {unsine :: N}
        | Cosine {uncosine :: N}
        | Tangent {untangent :: N}
        | Cotangent {uncotangent :: N}
        | Secant {unsecant :: N}
        | Cosecant {uncosecant :: N}
        | Arcsine {unarcsione :: N}
        | Arccosine {unarccosine :: N}
        | Arctangent {unarctangent :: N}
        | Arccotangent {unarccotangent :: N}
        | Arcsecant {unarcsecant :: N}
        | Arccosecant {unarccosecant :: N}
        | SeriesSum {summin :: Natural, summax :: Natural, sumf :: N}
        | SeriesProduct {prodmin :: Natural, prodmax :: Natural, prodf :: N}
        | Unknown
        | Inequality {eqtype :: Ordering, value1 :: N, value2 :: N}
        | Derivative {integral :: Natural}

data C = Pi | Phi | I | E

complete :: N -> Bool
complete (N _) = True
complete (C _) = True
complete NaN = False
complete (Sum p q) = complete p && complete q
complete Unknown = False