module MaybeX where

allow :: Bool -> b -> Maybe b
allow True   = Just
allow False  = const Nothing
