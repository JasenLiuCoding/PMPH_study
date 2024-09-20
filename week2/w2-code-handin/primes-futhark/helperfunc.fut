------------------------
--- exclusive scan ---
------------------------
let scan_exc [n] 't (op: t -> t -> t) (ne: t) (arr: [n]t) :[n]t = 
  let s_inc = scan (op) ne arr
  let dp = init s_inc
  let res = [ne] ++ dp
  in res :> [n]t
------------------------
--- Sgm Scan Helpers ---
------------------------
-- Generic segmented scan (generic in the binary operator and in the element
-- type, t, of the segmented array).
let sgmScan [n] 't (op: t -> t -> t) (ne: t)
                   (flags: [n]i64) (arr: [n]t) : [n]t =
  let (_, res) =
    scan (\(x_flag,x) (y_flag,y) -> -- extended binop is denoted $\odot$
             let fl = x_flag | y_flag
             let vl = if y_flag != 0 then y else op x y
             in  (fl, vl)
         ) (0, ne) (zip flags arr)
    |> unzip
  in  res
let sgmScan_inc [n] 't
            (op: t -> t -> t)
            (ne: t)
            (flags: [n]bool)
            (vals: [n]t)
            : [n]t =
  scan (\(f1, v1) (f2, v2) -> (f1 || f2, if f2 then v2 else op v1 v2))
       (false, ne)
       (zip flags vals)
  |> unzip
  |> (.1)
-- let sgmScan_exc [n] 't
--             (op: t -> t -> t)
--             (ne: t)
--             (flags: [n]bool)
--             (vals: [n]t)
--             : []t =
--   scan_exc (\(f1, v1) (f2, v2) -> (f1 || f2, if f2 then v2 else op v1 v2))
--        (false, ne)
--        (zip flags vals)
--   |> unzip
--   |> (.1)

------------------------
--- flag maker ---
------------------------
let mkFlagArray 't [m] 
            (aoa_shp: [m]i64) (zero: t)   --aoa_shp=[0,3,1,0,4,2,0]
            (aoa_val: [m]t  ) : []t   =   --aoa_val=[1,1,1,1,1,1,1]
  let shp_rot = map (\i->if i==0 then 0   --shp_rot=[0,0,3,1,0,4,2]
                         else aoa_shp[i-1]
                    ) (iota m)
  let shp_scn = scan (+) 0 shp_rot       --shp_scn=[0,0,3,4,4,8,10]
  let aoa_len = if m == 0 then 0         --aoa_len= 10
                else shp_scn[m-1]+aoa_shp[m-1]
  let shp_ind = map2 (\shp ind ->        --shp_ind= 
                       if shp==0 then -1 --  [-1,0,3,-1,4,8,-1]
                       else ind          --scatter
                     ) aoa_shp shp_scn   --   [0,0,0,0,0,0,0,0,0,0]
  in scatter (replicate aoa_len zero)    --   [-1,0,3,-1,4,8,-1]
             shp_ind aoa_val             --   [1,1,1,1,1,1,1]
                                     -- res = [1,0,0,1,1,0,0,0,1,0] 