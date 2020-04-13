type vec3 = {x: f32, y: f32, z: f32 }

-- A convenient alias so we don't have to indicate the fields all the
-- time.
let vec (x, y, z) : vec3 = {x,y,z}

let vf f (v1: vec3) (v2: vec3) =
  {x= f v1.x v2.x,
   y= f v1.y v2.y,
   z= f v1.z v2.z}

let vec_add = vf (+)
let vec_sub = vf (-)
let vec_mul = vf (*)
let vec_div = vf (/)

let scale s (v: vec3) =
  { x=s*v.x
  , y=s*v.y
  , z=s*v.z }

let dot (v1: vec3) (v2: vec3) =
  let v3 = vec_mul v1 v2
  in v3.x + v3.y + v3.z

let norm v = f32.sqrt (dot v v)

let normalise v = scale (1.0 / norm v) v

let cross (v1: vec3) (v2: vec3) =
  { x=v1.y*v2.z-v1.z*v2.y
  , y=v1.z*v2.x-v1.x*v2.z
  , z=v1.x*v2.y-v1.y*v2.x }

-- | Axis-aligned bounding box.
type aabb = { min: vec3, max: vec3 }

let enclosing (box0: aabb) (box1: aabb) : aabb =
  let small = vec(f32.min box0.min.x box1.min.x,
                  f32.min box0.min.y box1.min.y,
                  f32.min box0.min.z box1.min.z)
  let big = vec(f32.max box0.max.x box1.max.x,
                f32.max box0.max.y box1.max.y,
                f32.max box0.max.z box1.max.z)
  in {min = small, max = big}

let centre ({min, max}: aabb) =
  {x=min.x + (max.x - min.x),
   y=min.y + (max.y - min.y),
   z=min.z + (max.z - min.z)}
