{-# LANGUAGE DataKinds #-}
module AES.GF28(mult) where

import Prelude (($))
import ReWire
import ReWire.Bits           as B
import ReWire.Finite
import ReWire.FiniteComp     as FC hiding ((*))
import ReWire.Vectors hiding ((++))

distrib :: W 8 -> W 8 -> Finite 8 -> W 16
distrib w m i = if b
                 then resize w <<. fromFinite i
                 else lit 0
   where
     b :: Bit
     b = m `index` (finite 7 FC.- i)
     
w , m :: W 8
w = lit 0x57
m = lit 0x13

pmul :: W 8 -> W 8 -> W 16
pmul w m = distrib w m (finite 0) ^
           distrib w m (finite 1) ^
           distrib w m (finite 2) ^
           distrib w m (finite 3) ^
           distrib w m (finite 4) ^
           distrib w m (finite 5) ^
           distrib w m (finite 6) ^
           distrib w m (finite 7)          

irreducible :: W 16
-- irreducible = lit 0x13
irreducible = lit 0x11b

-- /**
--  * The irreducible polynomial used in multiplication.
--  * [FIPS-197u1] Section 4.2, Algorithm 4.3
--  */
-- irreducible = <| x^^8 + x^^4 + x^^3 + x + 1 |>

modp :: Finite 8 -> W 8 -> W 16 -> W 16
modp i h w = if b
               then w ^ (irreducible <<. fromFinite i)
               else w
  where
    b :: Bit
    b = h `index` (finite 7 FC.- i)

pmod :: W 16 -> W 8
pmod w = drop $
         modp (finite 7) h $
         modp (finite 6) h $
         modp (finite 5) h $
         modp (finite 4) h $
         modp (finite 3) h $
         modp (finite 2) h $
         modp (finite 1) h $ 
         modp (finite 0) h w
  where
    h :: W 8
    h = take w

mult :: W 8 -> W 8 -> W 8
mult w m = pmod (pmul w m)

