module Option where

import Data.Function ((&))
import Data.Maybe (mapMaybe)

induce :: [a -> Maybe b] -> a -> [b]
induce options a = mapMaybe (a &) options
