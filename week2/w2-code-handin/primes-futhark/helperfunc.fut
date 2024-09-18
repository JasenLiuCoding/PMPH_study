------------------------
--- exclusive scan ---
------------------------
let scan_exc [n] 't (op: t -> t -> t) (ne: t) (arr: [n]t) :[]t = 
  let s_inc = scan (op) ne arr
  let dp = init s_inc
  let res = (([ne] ++ dp) :> [n]t)
  in res 
------------------------
--- Sgm Scan Helpers ---
------------------------
-- Generic segmented scan (generic in the binary operator and in the element
-- type, t, of the segmented array).
let sgmScan [n] 't
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
let sgmScan_exc [n] 't
            (op: t -> t -> t)
            (ne: t)
            (flags: [n]bool)
            (vals: [n]t)
            : []t =
  scan_exc (\(f1, v1) (f2, v2) -> (f1 || f2, if f2 then v2 else op v1 v2))
       (false, ne)
       (zip flags vals)
  |> unzip
  |> (.1)

------------------------
--- flag maker ---
------------------------
let mkFlagArray 't [m] (aoa_shp: [m]i64) (zero: t) (aoa_val: [m]t) : []t = 
  let shp_rot = map (\i -> if i==0 then 0
                           else aoa_shp[i-1]
                           ) (iota m)
  let shp_scn = scan (+) 0 shp_rot
  let aoa_len = if m == 0 then 0 
                else shp_scn[m-1] + aoa_shp[m-1]
  let shp_ind = map2 (\shp ind ->
                        if shp==0 then -1
                        else ind
                        ) aoa_shp shp_scn
  in scatter (replicate aoa_len zero)
             shp_ind aoa_val