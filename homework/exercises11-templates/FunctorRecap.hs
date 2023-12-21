{-# LANGUAGE InstanceSigs #-}

data TwoOrThree a = Two a a | Three a a a
  deriving (Eq, Show)

-- instance Functor TwoOrThree

-- fmap succ (Two 1 2)} =

-- doubleAll :: [TwoOrThree Int] -> [TwoOrThree Int]


data TwoOrThree' a = Two' Int a | Three' Int a a
  deriving (Eq, Show)

-- instance Functor TwoOrThree'

-- fmap succ (Two' 1 2) =

-- instance Functor Int

