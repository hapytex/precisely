module Precisely.MachinePrecision where

class HasMachinePrecision a where
    machineError :: a -> a

-- instance HasMachinePrecision Double where
--   machineError :: a -> a
