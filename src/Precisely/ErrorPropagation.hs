{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Precisely.ErrorPropagation where

import Data.Function(on)

-- In general for a value x, with error x', the error after applying a function
-- is y' = f'(x) * x'.
--
-- For a function with multiple variables, it is (d/dx_i f(x))

data Precise a e = Precise {
    pValue :: a
  , pError :: e
  } deriving (Eq, Functor, Ord, Show)

instance Num ae => Num (Precise ae ae) where
    Precise x x' + Precise y y' = Precise (x + y) (x' + y')
    Precise x x' - Precise y y' = Precise (x - y) (x' + y')
    Precise x x' * Precise y y' = Precise (x * y) (abs y * x' + abs x * y')
    abs (Precise x x') = Precise (abs x) x'
    fromInteger = (`Precise` 0) . fromInteger
    negate (Precise x x') = Precise (negate x) x'
    signum (Precise x x') = Precise (signum x) x'

instance Fractional ae => Fractional (Precise ae ae) where
    Precise x x' / Precise y y' = Precise (x/y) (x'/abs y + y'/(y*y))
    fromRational = (`Precise` 0) . fromRational
    recip (Precise x x') = Precise (recip x) (x'/(x*x))

_invDerivArcsincos :: Floating a => a -> a
_invDerivArcsincos x = sqrt (1 - x*x)

instance Floating ae => Floating (Precise ae ae) where
    pi = Precise pi 0
    exp (Precise x x') = Precise (exp x) (x' * exp x)
    log (Precise x x') = Precise (log x) (x'/abs x)
    sqrt (Precise x x') = Precise (sqrt x) (x'/(2*sqrt x))
    sin (Precise x x') = Precise (sin x) (x' * abs (cos x))
    cos (Precise x x') = Precise (cos x) (x' * abs (sin x))
    tan (Precise x x') = Precise (tan x) (x' / abs (sc * sc))
        where sc = cos x
    asin (Precise x x') = Precise (asin x) (x' / _invDerivArcsincos x)
    acos (Precise x x') = Precise (acos x) (x' / _invDerivArcsincos x)
    atan (Precise x x') = Precise (atan x) (x' / (1 + x*x))
    sinh (Precise x x') = Precise (sinh x) (x' * abs (cosh x))
    cosh (Precise x x') = Precise (cosh x) (x' * abs (sinh x))
    tanh (Precise x x') = Precise (tanh x) (x' / abs (sc * sc))
        where sc = cosh x
