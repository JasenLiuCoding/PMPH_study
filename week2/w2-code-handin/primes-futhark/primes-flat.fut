-- Primes: Flat-Parallel Version
-- ==
-- compiled input { 30i64 } output { [2i64, 3i64, 5i64, 7i64, 11i64, 13i64, 17i64, 19i64, 23i64, 29i64] }
-- compiled input { 10000000i64 }

-- output @ ref10000000.out
import "helperfunc"

let primesFlat (n : i64) : []i64 =
  let sq_primes   = [2i64, 3i64, 5i64, 7i64]
  let len  = 8i64
  let (sq_primes, _) =
    loop (sq_primes, len) while len < n do
      -- this is "len = min n (len*len)" 
      -- but without running out of i64 bounds 
      let len = if n / len < len then n else len*len

      let mult_lens = map (\ p -> (len / p) - 1 ) sq_primes
      let flat_size = reduce (+) 0 mult_lens

      --------------------------------------------------------------
      -- The current iteration knows the primes <= 'len', 
      --  based on which it will compute the primes <= 'len*len'
      -- ToDo: replace the dummy code below with the flat-parallel
      --       code that is equivalent with the nested-parallel one:
      --
      --   let composite = map (\ p -> let mm1 = (len / p) - 1
      --                               in  map (\ j -> j * p ) (map (+2) (iota mm1))
      --                       ) sq_primes
      --   let not_primes = reduce (++) [] composite
      --
      -- Your code should compute the correct `not_primes`.
      -- Please look at the lecture slides L2-Flattening.pdf to find
      --  the normalized nested-parallel version.
      -- Note that the scalar computation `mm1 = (len / p) - 1' has
      --  already been distributed and the result is stored in "mult_lens",
      --  where `p \in sq_primes`.
      -- Also note that `not_primes` has flat length equal to `flat_size`
      --  and the shape of `composite` is `mult_lens`. 
      let composite = 
        -- let iot = iota mult_lens
        let iots = 
          -- let len = length mult_lens
          -- let flag = mkFlagArray mult_lens 0 mult_lens
          -- let vals = map (\f -> if f!=0 then 0 else 1) flag
          -- in sgmScan_inc (+) 0 (flag :> [len]i64) (vals :> [len]i64)
          let flag = mkFlagArray mult_lens 0 mult_lens
          let vals = map (\f -> if f!=0 then 0 else 1) flag
          in sgmScan_inc (+) 0 (flag :> [flat_size]i64) (vals :> [flat_size]i64)


        -- let twom = map(\p -> map(+2) iot) sqrt_primes
        let twoms = map (\i -> i+2) iots 

        -- let rp = map (\p -> replicate mult_lens p)
        let rps = 
          let (flag_n, flag_v) = zip mult_lens sq_primes |> mkFlagArray mult_lens (0,0) |> unzip
          in sgmScan_inc (+) 0 flag_n flag_v
        
        in map (\(j,p) -> j*p) (zip twoms rps)

      
      

      

      
      


      -- let not_primes = replicate flat_size 0
      let not_primes = reduce (++) [] composite

      -- If not_primes is correctly computed, then the remaining
      -- code is correct and will do the job of computing the prime
      -- numbers up to n!
      --------------------------------------------------------------
      --------------------------------------------------------------

       let zero_array = replicate flat_size 0i8
       let mostly_ones= map (\ x -> if x > 1 then 1i8 else 0i8) (iota (len+1))
       let prime_flags= scatter mostly_ones not_primes zero_array
       let sq_primes = filter (\i-> (i > 1i64) && (i <= n) && (prime_flags[i] > 0i8))
                              (0...len)

       in  (sq_primes, len)

  in sq_primes

-- RUN a big test with:
--   $ futhark cuda primes-flat.fut
--   $ echo "10000000i64" | ./primes-flat -t /dev/stderr -r 10 > /dev/null
-- or simply use futhark bench, i.e.,
--   $ futhark bench --backend=cuda primes-flat.fut
let main (n : i64) : []i64 = primesFlat n
