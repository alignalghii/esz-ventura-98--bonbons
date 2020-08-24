module ShowInd where

class ShowInd a where
	showInd :: Int -> a -> ShowS
