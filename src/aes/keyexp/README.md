
-- |
-- | AES-256 instance
-- | N.b., in Fig. 11, w[8] depends on w[7], then w[9] depends on w[8], etc., and,
-- | consequently, this can't be written as a map, Finite 60 -> W 32. Or, in other words,
-- | it is truly iterative.
-- |

-- | This is for defining the KeyExpansion routine from Fig 11, page 20, of nist.fips.197.
-- 
--           Nk   Nb   Nr   KeyExpansion(byte key[4*nk] , word w[nb*(nr+1)])
-- -------------------------------------------------------------------------
-- AES-128 |  4 |  4 | 10 | KeyExpansion(byte key[16] , word w[44])
-- -------------------------------------------------------------------------
-- AES-192 |  6 |  4 | 12 | KeyExpansion(byte key[24] , word w[52])
-- -------------------------------------------------------------------------
-- AES-256 |  8 |  4 | 14 | KeyExpansion(byte key[32] , word w[60])

-- | To sort that out, here are the types for the
-- | various key sizes.
-- -------------------------------------------------------
-- AES-128 | word key[4] , word w[44] | 44 - 4 = 40
-- -------------------------------------------------------
-- AES-192 | word key[6] , word w[52] | 52 - 6 = 46
-- -------------------------------------------------------
-- AES-256 | word key[8] , word w[60] | 60 - 8 = 52
-- -------------------------------------------------------
