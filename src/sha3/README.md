This code started originally in my (private) pqc repo.

This design is drawn directly from the SHA-3 Standard:
  * https://nvlpubs.nist.gov/nistpubs/fips/nist.fips.202.pdf

In particular, if you look at Table 1 on page 7 of the above, this design
hardcodes specific values:
  * b = 1600
  * w = 64, and
  * l = 6

To generate test vectors in a file, type, for example:
   > cryptol StepOperations.cry -b gen_rho > testvectors/rho1000.txt
where the contents of ./gen_rho is:
   > testVectors rho

I've been putting the test vectors in the directory testvectors. The type of a battery of test vectors (like rho1000.txt) is a value of type: 
   > [([[Integer]], [[Integer]])]
Within Test.hs, there is a function which converts such values into [(A,A)],
where the first component of each pair is an input state (A) and the second is the output computed by the Cryptol gold definition in StepOperations.cry. 

 * StepOperations.cry defines all of the Keccak step operations as snarfed from the cryptol-specs repo. 
 * Each of these step operations (theta, rho, etc.) has a ReWire definition in a separate file (resp., Theta.hs, Rho.hs, etc.).
 * The Haskell program Test.hs imports these ReWire definitions, ingests 
 test batteries computed by Cryptol, and ensures, for any (i,o) in that 
 battery and ReWire step operation f, that f i == o.
 * Each of the ReWire step operations and the round-function composition, `rnd`, tested correctly against 1000-case test 
 batteries. The call looks like:
> λ> test theta "testvectors/theta1000.txt"
>    Just [True,<...snip...>,True]
> λ> 
 
There is, no doubt, a slicker, mostly automated way to do what I've done here, but this works pretty well. 
