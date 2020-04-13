import "lib/github.com/diku-dk/sorts/radix_sort"

import "radixtree"
import "prim"

-- | Expands a 10-bit integer into 30 bits by inserting 2 zeros after
-- each bit.
let expand_bits (v: u32) : u32 =
  let v = (v * 0x00010001) & 0xFF0000FF
  let v = (v * 0x00000101) & 0x0F00F00F
  let v = (v * 0x00000011) & 0xC30C30C3
  let v = (v * 0x00000005) & 0x49249249
  in v

let morton_3D {x,y,z} : u32 =
  let x = f32.min (f32.max(x * 1024) 0) 1023
  let y = f32.min (f32.max(y * 1024) 0) 1023
  let z = f32.min (f32.max(z * 1024) 0) 1023
  let xx = expand_bits (u32.f32 x)
  let yy = expand_bits (u32.f32 y)
  let zz = expand_bits (u32.f32 z)
  in xx * 4 + yy * 2 + zz

type ptr = #leaf i32 | #inner i32

type inner = {aabb: aabb, left:ptr, right:ptr, parent:i32}

type~ bvh [n] 't = {L: [n]t, I: []inner}

let bvh_mk [n] 't (bbf: t -> aabb) (ts: [n]t) : bvh [n] t =
  let centers = map (bbf >-> centre) ts
  let x_max = f32.maximum (map (.x) centers)
  let y_max = f32.maximum (map (.y) centers)
  let z_max = f32.maximum (map (.z) centers)
  let x_min = f32.minimum (map (.x) centers)
  let y_min = f32.minimum (map (.y) centers)
  let z_min = f32.minimum (map (.z) centers)
  let normalise {x,y,z} = {x=(x-x_min)/(x_max-x_min),
                           y=(y-y_min)/(y_max-y_min),
                           z=(z-z_min)/(z_max-z_min)}
  let morton = bbf >-> centre >-> normalise >-> morton_3D

  let ts = radix_sort_by_key morton u32.num_bits u32.get_bit ts
  let empty_aabb = {min = vec(0,0,0), max = vec(0,0,0)}
  let empty_aabb {left, right, parent} = {aabb=empty_aabb, left, right, parent}
  let inners = map empty_aabb (mk_radix_tree (map morton ts))
  let depth = t32 (f32.log2 (r32 n)) + 2
  let get_aabb inners ptr =
    match ptr
    case #leaf i -> bbf (unsafe ts[i])
    case #inner i -> unsafe inners[i].aabb
  let update inners {aabb=_, left, right, parent} =
    {aabb = enclosing (get_aabb inners left) (get_aabb inners right),
     left,
     right,
     parent}
  let inners = loop inners for _i < depth do
               map (update inners) inners
  in {L = ts, I = inners}

let bvh_fold [n] 'a 'b (contains: aabb -> bool) (op: b -> i32 -> a -> b) (init: b) (t: bvh [n] a) : b =
  (.0) <|
  loop (acc, cur, prev) = (init, 0, #inner (-1))
  while cur != -1 do
  let node = unsafe t.I[cur]
  let from_left = prev == node.left
  let from_right = prev == node.right
  let rec_child : #rec ptr | #norec =
    -- Did we return from left node?
    if from_left
    then #rec node.right
    -- First encounter and in this BB?
    else if !from_right
    then if contains node.aabb
         then #rec node.left
         else #norec
    else #norec
  in match rec_child
     case #norec ->
       (acc, node.parent, #inner cur)
     case #rec ptr ->
       match ptr
       case #inner i -> (acc, i, #inner cur)
       case #leaf i -> (op acc i (unsafe t.L[i]), cur, ptr)
