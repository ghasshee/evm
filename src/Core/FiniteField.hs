{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DeriveDataTypeable #-}

module Core.FiniteField where 

import Data.Modular
import Data.List
import Data.FiniteField.PrimeField as P
import Core.Config as C

data ECPoint a  = P a a deriving (Eq,Ord,Show)
type Fp         = $(primeField C.prime)

class (Num k) => ModOnR a k where 
    (&)         :: k -> a -> a
instance (Ord k,Eq k,Num k,Integral k) => ModOnR (ECPoint Fp) k where
    n & p       = 
            if n==1 then p else 
            if odd n    then p +/ ((div(n-1)2) & (p +/ p)) 
                        else (div n 2) & (p +/ p) 

g :: ECPoint Fp
g = P 8077 5711 
a :: forall x. (Eq x,Fractional x) => x  
a = 90
b = 1 
f x = x^3 + a*x + b

x (P a b) = a
y (P a b) = b 

λ           :: forall x. (Eq x, Fractional x) => ECPoint x -> ECPoint x -> x 
λ p q       = if p==q then pp else pq    where  pp = (3*(x p)^2 + a) / (y p) 
                                                pq = (y p-y q) / (x p-x q)
(+/) p q   = P xr yr    where   xr = (λ p q)^2-x p-x q
                                yr = (-1)*((λ p q)*(xr-x p)+y p)
p = fromInteger C.prime  
list = sort ([P x y| x<-[0..p-1], y<-[0..p-1] , y^2 == f x] :: [ECPoint Fp] )
toInt = P.toInteger
