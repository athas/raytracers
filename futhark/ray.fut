import "prim"

type pos = vec3
type dir = vec3
type colour = vec3

let black : vec3 = {x=0.0, y=0.0, z=0.0}
let white : vec3 = {x=1.0, y=1.0, z=1.0}

type ray = {origin: vec3,
            dir: vec3}

let point_at_param (ray: ray) t =
  ray.origin `vec_add` scale t ray.dir

type hit = { t: f32
           , p: pos
           , normal: dir
           , colour: colour }

type sphere = { pos: pos
              , colour: colour
              , radius: f32 }

type opt 'a = #some a | #none

let sphere_aabb (s: sphere) : aabb =
  { min = s.pos `vec_sub` {x=s.radius, y=s.radius, z=s.radius}
  , max = s.pos `vec_add` {x=s.radius, y=s.radius, z=s.radius}}

let sphere_hit (s: sphere) r t_min t_max : opt hit =
  let oc = vec_sub r.origin s.pos
  let a = dot r.dir r.dir
  let b = dot oc r.dir
  let c = dot oc oc - s.radius*s.radius
  let discriminant = b*b - a*c
  let f temp =
    if temp < t_max && temp > t_min
    then #some { t = temp
               , p = point_at_param r temp
               , normal = scale (1.0/s.radius)
                                (point_at_param r temp `vec_sub` s.pos)
               , colour = s.colour
               }
               else #none
  in if discriminant <= 0.0
     then #none
     else match f ((-b - f32.sqrt(b*b-a*c))/a)
          case #some hit -> #some hit
          case #none -> f ((-b + f32.sqrt(b*b-a*c))/a)

let aabb_hit (aabb: aabb) (r: ray) tmin0 tmax0 =
  let iter min' max' origin' dir' tmin' tmax' =
    let invD = 1.0 / dir'
    let t0 = (min' - origin') * invD
    let t1 = (max' - origin') * invD
    let (t0', t1') = if invD < 0.0 then (t1, t0) else (t0, t1)
    let tmin'' = f32.max t0' tmin'
    let tmax'' = f32.min t1' tmax'
    in (tmin'', tmax'')
  let (tmin1, tmax1) =
    iter aabb.min.x aabb.max.x r.origin.x r.dir.x tmin0 tmax0
  in if tmax1 <= tmin1 then false
     else let (tmin2, tmax2) =
            iter aabb.min.y aabb.max.y r.origin.y r.dir.y tmin1 tmax1
          in if tmax2 <= tmin2 then false
             else let (tmin3, tmax3) =
                    iter aabb.min.z aabb.max.z r.origin.z r.dir.z tmin2 tmax2
                  in !(tmax3 <= tmin3)

import "bvh"

type~ objs = bvh [] sphere

let objs_hit (bvh: objs) (r: ray) (t_min: f32) (t_max: f32) : opt hit =
  let contains aabb = aabb_hit aabb r t_min t_max
  let closest_hit (j, t_max) i s =
    match sphere_hit s r t_min t_max
    case #none -> (j, t_max)
    case #some h -> (i, h.t)
  let (j, t_max) = bvh_fold contains closest_hit (-1, t_max) bvh
  in if j >= 0
     then let s = unsafe bvh.L[j]
          in sphere_hit s r t_min (t_max+1)
     else #none

type camera = { origin: pos
              , llc: pos
              , horizontal: dir
              , vertical: dir }

let camera lookfrom lookat vup vfov aspect =
  let theta = vfov * f32.pi / 180.0
  let half_height = f32.tan (theta / 2.0)
  let half_width = aspect * half_height
  let origin = lookfrom
  let w = normalise (lookfrom `vec_sub` lookat)
  let u = normalise (cross vup w)
  let v = cross w u
  in { origin = lookfrom
     , llc = origin `vec_sub` scale half_width u
                    `vec_sub` scale half_height v
                    `vec_sub` w
     , horizontal = scale (2.0*half_width) u
     , vertical = scale (2.0*half_height) v
     }

let get_ray (cam: camera) s t : ray =
  { origin = cam.origin
  , dir = cam.llc `vec_add` scale s cam.horizontal
                  `vec_add` scale t cam.vertical
                  `vec_sub` cam.origin
  }

let reflect v n =
  v `vec_sub` scale (2.0 * dot v n) n

let scatter (r: ray) (hit: hit) : opt (ray, colour) =
  let reflected = reflect (normalise r.dir) hit.normal
  let scattered = {origin = hit.p, dir = reflected}
  in if dot scattered.dir hit.normal > 0.0
     then #some (scattered, hit.colour)
     else #none

let ray_colour objs r (max_depth: i32) =
  (.3) <|
  loop (r, depth, light, colour) = (r, 0, vec(1,1,1), vec(0,0,0))
  while depth < max_depth do
    match objs_hit objs r 0.001 1000000000.0
    case #some hit ->
      (match scatter r hit
       case #some (scattered, attenuation) ->
         (scattered, depth + 1,
          light `vec_mul` attenuation,
          light `vec_mul` colour)
       case #none ->
         (r, max_depth,
          light,
          light `vec_mul` colour))
    case #none ->
      let unit_dir = normalise r.dir
      let t = 0.5 * (unit_dir.y + 1.0)
      let bg = {x=0.5, y=0.7, z=1.0}
      in (r, max_depth,
          light,
          light `vec_mul`
          (scale (1.0-t) white `vec_add` scale t bg))

let trace_ray objs width height cam j i : colour =
  let u = r32 i / r32 width
  let v = r32 j / r32 height
  let ray = get_ray cam u v
  in ray_colour objs ray 50

type pixel = i32

let colour_to_pixel (p: colour) : pixel =
  let ir = i32.f32 (255.99 * p.x)
  let ig = i32.f32 (255.99 * p.y)
  let ib = i32.f32 (255.99 * p.z)
  in (ir << 16) | (ig << 8) | ib

type image [h][w] = [h][w]pixel

let render objs width height cam : image [height][width] =
  let pixel j i =
    colour_to_pixel (trace_ray objs width height cam (height-j) i)
  in tabulate_2d height width pixel

type~ scene = { look_from: pos
              , look_at: pos
              , fov: f32
              , spheres: []sphere }

let from_scene width height (scene: scene) : (objs, camera) =
  (bvh_mk sphere_aabb scene.spheres,
   camera scene.look_from scene.look_at {x=0.0, y=1.0, z=0.0}
          scene.fov (r32 width/r32 height))

let rgbbox : scene =
  let n = 10
  let k = 60.0

  let leftwall =
    flatten <|
    tabulate_2d n n (\y z ->
                       { pos={x=(-k/2.0),
                              y=(-k/2.0 + (k/r32 n) * r32 y),
                              z=(-k/2.0 + (k/r32 n) * r32 z)}
                       , colour={x=1.0, y=0.0, z=0.0}
                       , radius = (k/(r32 n*2.0))})

  let midwall =
    flatten <|
    tabulate_2d n n (\x y ->
                       { pos={x=(-k/2.0 + (k/r32 n) * r32 x),
                              y=(-k/2.0 + (k/r32 n) * r32 y),
                              z=(-k/2.0)}
                       , colour={x=1.0, y=1.0, z=0.0}
                       , radius = (k/(r32 n*2.0))})

  let rightwall =
    flatten <|
    tabulate_2d n n (\y z ->
                       { pos={x=(k/2.0),
                              y=(-k/2.0 + (k/r32 n) * r32 y),
                              z=(-k/2.0 + (k/r32 n) * r32 z)}
                       , colour={x=0.0, y=0.0, z=1.0}
                       , radius = (k/(r32 n*2.0))})


  let bottom =
    flatten <|
    tabulate_2d n n (\x z ->
                       { pos={x=(-k/2.0 + (k/r32 n) * r32 x),
                              y=(-k/2.0),
                              z=(-k/2.0 + (k/r32 n) * r32 z)}
                       , colour={x=1.0, y=1.0, z=1.0}
                       , radius = (k/(r32 n*2.0))})


  in { spheres = leftwall ++ midwall ++ rightwall ++ bottom
     , look_from = {x=0.0, y=30.0, z=30.0}
     , look_at = {x=0.0, y= -1.0, z= -1.0}
     , fov = 75.0 }

let irreg : scene =
    let n = 100
    let k = 600.0
    let bottom =
      flatten <|
      tabulate_2d n n (\x z ->
                         { pos={x=(-k/2.0 + (k/r32 n) * r32 x),
                                y=0.0,
                                z=(-k/2.0 + (k/r32 n) * r32 z)}
                         , colour = white
                         , radius = k/(r32 n * 2.0)})
    in { spheres = bottom
       , look_from = {x=0.0, y=12.0, z=30.0}
       , look_at = {x=0.0, y=10.0, z= -1.0}
       , fov = 75.0 }

-- ==
-- entry: bvh_rgbbox bvh_irreg render_rgbbox render_irreg
-- input { 1000 1000 }

entry bvh_rgbbox h w = from_scene h w rgbbox
entry bvh_irreg h w = from_scene h w irreg

entry render_rgbbox h w =
  let (objs, cam) = from_scene w h rgbbox
  in render objs w h cam

entry render_irreg h w =
  let (objs, cam) = from_scene w h irreg
  in render objs w h cam
