module HW0.T3
  ( compose,
    contract,
    i,
    k,
    permute,
    s,
  )
where
  
-- Substitution
s :: (a -> b -> c) -> (a -> b) -> (a -> c)
s f g x = f x (g x)

-- Kestrel
-- { k = const }
k :: a -> b -> a
k x _ = x

-- Identity
-- { i = id }
i :: a -> a
i = s k k

-- Bluebird
-- { compose = (.) }
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose = s (k s) k

-- Warbler
-- { contract f x = f x x } 
contract :: (a -> a -> b) -> (a -> b)
contract = s s (s k)

-- Cardinal
-- { permute = flip }
permute :: (a -> b -> c) -> (b -> a -> c)
permute = s (s (k s) (s (k k) s)) (k k)
