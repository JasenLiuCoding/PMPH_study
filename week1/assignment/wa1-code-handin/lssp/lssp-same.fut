-- Parallel Longest Satisfying Segment
--
-- Dataset 1: given
-- ==
-- compiled input {
--    [1i32, -2i32, -2i32, 0i32, 0i32, 0i32, 0i32, 0i32, 3i32, 4i32, -6i32, 1i32]
-- }
-- output {
--    5i32
-- }
-- Dataset 2: 1 to 10
-- ==
-- compiled input {
--    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
-- }
-- output {
--    1
-- }
-- Dataset 3: 1 to 10; 7 repeat
-- ==
-- compiled input {
--    [1, 2, 3, 4, 5, 6, 7, 7, 8, 9, 10]
-- }
-- output {
--    2
-- }

import "lssp"
import "lssp-seq"

let main (xs: []i32) : i32 =
  let pred1 _x = true
  let pred2 x y = (x == y)
--  in  lssp_seq pred1 pred2 xs
  in  lssp pred1 pred2 xs
