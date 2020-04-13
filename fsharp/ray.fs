open System
open System.Diagnostics
open System.Threading
open System.Threading.Tasks

type vec3 = {x: float
             y: float
             z: float }

let inline vf f (v1: vec3) (v2: vec3) =
    {x= f v1.x v2.x;
     y= f v1.y v2.y;
     z= f v1.z v2.z}

let inline vec_add (v1: vec3) (v2: vec3) =
    {x= v1.x + v2.x;
     y= v1.y + v2.y;
     z= v1.z + v2.z}
let inline vec_sub (v1: vec3) (v2: vec3) =
    {x= v1.x - v2.x;
     y= v1.y - v2.y;
     z= v1.z - v2.z}
let inline vec_mul (v1: vec3) (v2: vec3) =
    {x= v1.x * v2.x;
     y= v1.y * v2.y;
     z= v1.z * v2.z}
let inline vec_div (v1: vec3) (v2: vec3) =
    {x= v1.x / v2.x;
     y= v1.y / v2.y;
     z= v1.z / v2.z}

let inline scale s v : vec3 =
    { x=s*v.x
    ; y=s*v.y
    ; z=s*v.z }

let inline dot (v1: vec3) (v2: vec3) =
    let v3 = vec_mul v1 v2
    in v3.x + v3.y + v3.z

let inline norm v = Math.Sqrt (dot v v)

let inline normalise v = scale (1.0 / norm v) v

let inline cross v1 v2 : vec3 =
    { x=v1.y*v2.z-v1.z*v2.y
    ; y=v1.z*v2.x-v1.x*v2.z
    ; z=v1.x*v2.y-v1.y*v2.x }

type aabb = { min: vec3
              max: vec3 }

let enclosing (box0: aabb) (box1: aabb) =
    let small = { x = min box0.min.x box1.min.x
                ; y = min box0.min.y box1.min.y
                ; z = min box0.min.z box1.min.z
                }
    let big = { x = max box0.max.x box1.max.x
              ; y = max box0.max.y box1.max.y
              ; z = max box0.max.z box1.max.z
              }
    in {min=small; max=big}

let inline centre (aabb: aabb) =
    { x = aabb.min.x + aabb.max.x - aabb.min.x
    ; y = aabb.min.y + aabb.max.y - aabb.min.y
    ; z = aabb.min.z + aabb.max.z - aabb.min.z
    }

type 'a bvh =
    | Bvh_leaf of aabb * 'a
    | Bvh_split of aabb * bvh<'a> * bvh<'a>

let bvh_aabb bvh =
    match bvh with
        | (Bvh_leaf (box, _)) -> box
        | (Bvh_split (box, _, _)) -> box

let rec split n xs =
    match (n, xs) with
        | (0, _) -> ([], xs)
        | (_, []) -> ([], [])
        | (_, x::xs') ->
            let (left, right) = split (n-1) xs'
            in (x::left, right)

let inline axis d aabb =
    let v = centre aabb
    in match d % 3 with
        | 0 -> v.x
        | 1 -> v.y
        | _ -> v.z

let mk_bvh f all_objs =
    let rec mk d n xs =
        match xs with
            | [] -> failwith "mk_bvh: no nodes"
            | [x] -> Bvh_leaf(f x, x)
            | _ ->
            let key x = axis d (f x)
            let xs_sorted = List.sortBy key xs
            let (xs_left, xs_right) = split (n/2) xs_sorted
            let do_left () = mk (d+1) (n/2) xs_left
            let do_right () = mk (d+1) (n-(n/2)) xs_right
            let (left, right) =
                if n < 100
                then (do_left(), do_right())
                else
                    let left_task =
                        Task.Factory.StartNew(do_left,
                                              TaskCreationOptions.None)
                    let right_task =
                        Task.Factory.StartNew(do_right,
                                              TaskCreationOptions.None)
                    in (left_task.Result, right_task.Result)
            let box = enclosing (bvh_aabb left) (bvh_aabb right)
            in Bvh_split (box, left, right)
    in mk 0 (List.length all_objs) all_objs

type pos = vec3
type dir = vec3
type colour = vec3

let black : vec3 = {x=0.0; y=0.0; z=0.0}
let white : vec3 = {x=1.0; y=1.0; z=1.0}

type ray = {origin: pos
            dir: dir}

let inline point_at_param (ray: ray) t =
    vec_add ray.origin (scale t ray.dir)

type hit = { t: float
             p: pos
             normal: dir
             colour: colour
           }

type sphere = { pos: pos
                colour: colour
                radius: float
              }

let sphere_aabb (s: sphere) : aabb =
    { min = vec_sub s.pos {x=s.radius; y=s.radius; z=s.radius}
    ; max = vec_add s.pos {x=s.radius; y=s.radius; z=s.radius}}

let sphere_hit s r t_min t_max : hit option =
    let oc = vec_sub r.origin s.pos
    let a = dot r.dir r.dir
    let b = dot oc r.dir
    let c = dot oc oc - s.radius*s.radius
    let discriminant = b*b - a*c
    let f temp =
            if temp < t_max && temp > t_min
            then Some { t = temp
                      ; p = point_at_param r temp
                      ; normal = scale (1.0/s.radius)
                                 (vec_sub (point_at_param r temp) s.pos)
                      ; colour = s.colour
                      }
            else None
    in if discriminant <= 0.0
       then None
       else match f ((-b - Math.Sqrt(b*b-a*c))/a) with
                | Some hit -> Some hit
                | None -> f ((-b + Math.Sqrt(b*b-a*c))/a)

let aabb_hit aabb (r: ray) tmin0 tmax0 =
    let inline iter min' max' origin' dir' tmin' tmax' =
        let invD = 1.0 / dir'
        let t0 = (min' - origin') * invD
        let t1 = (max' - origin') * invD
        let (t0', t1') = if invD < 0.0 then (t1, t0) else (t0, t1)
        let tmin'' = max t0' tmin'
        let tmax'' = min t1' tmax'
        in (tmin'', tmax'')
    let (tmin1, tmax1) =
        iter aabb.min.x aabb.max.x r.origin.x r.dir.x tmin0 tmax0
    in if tmax1 <= tmin1 then false
        else let (tmin2, tmax2) =
                 iter aabb.min.y aabb.max.y r.origin.y r.dir.y tmin1 tmax1
             in if tmax2 <= tmin2 then false
                else let (tmin3, tmax3) =
                         iter aabb.min.z aabb.max.z r.origin.z r.dir.z tmin2 tmax2
                     in not (tmax3 <= tmin3)

type objs = sphere bvh

let rec objs_hit bvh r t_min t_max =
    match bvh with
        | (Bvh_leaf (_, s)) ->
            sphere_hit s r t_min t_max
        | (Bvh_split (box, left, right)) ->
            if not (aabb_hit box r t_min t_max)
            then None
            else match objs_hit left r t_min t_max with
                     | Some h -> (match objs_hit right r t_min h.t with
                                      | None -> Some h
                                      | Some h' -> Some h')
                     | None -> objs_hit right r t_min t_max

type camera = { origin: pos
              ; llc: pos
              ; horizontal: dir
              ; vertical: dir
              }

let camera lookfrom lookat vup vfov aspect =
  let theta = vfov * Math.PI / 180.0
  let half_height = Math.Tan (theta / 2.0)
  let half_width = aspect * half_height
  let origin = lookfrom
  let w = normalise (vec_sub lookfrom lookat)
  let u = normalise (cross vup w)
  let v = cross w u
  in { origin = lookfrom
     ; llc = vec_sub
             (vec_sub (vec_sub origin (scale half_width u))
                     (scale half_height v)) w
     ; horizontal = scale (2.0*half_width) u
     ; vertical = scale (2.0*half_height) v
     }

let get_ray (cam: camera) s t : ray =
    { origin = cam.origin
    ; dir = vec_sub (vec_add (vec_add cam.llc (scale s cam.horizontal))
                             (scale t cam.vertical))
                    cam.origin
    }

let reflect v n =
    vec_sub v (scale (2.0 * dot v n) n)

let scatter (r: ray) (hit: hit) =
    let reflected = reflect (normalise r.dir) hit.normal
    let scattered = {origin = hit.p; dir = reflected}
    in if dot scattered.dir hit.normal > 0.0
       then Some (scattered, hit.colour)
       else None

let rec ray_colour objs r depth =
    match objs_hit objs r 0.001 1000000000.0 with
        | Some hit ->
            (match scatter r hit with
             | Some (scattered, attenuation) ->
             if depth < 50
             then vec_mul attenuation (ray_colour objs scattered (depth+1))
             else black
             | None -> black)
        | None ->
          let unit_dir = normalise r.dir
          let t = 0.5 * (unit_dir.y + 1.0)
          let bg = {x=0.5; y=0.7; z=1.0}
          in vec_add (scale (1.0-t) white) (scale t bg)

let trace_ray objs width height cam j i : colour =
    let u = float i / float width
    let v = float j / float height
    let ray = get_ray cam u v
    in ray_colour objs ray 0

type pixel = int * int * int

let colour_to_pixel p =
    let ir = int (255.99 * p.x)
    let ig = int (255.99 * p.y)
    let ib = int (255.99 * p.z)
    in (ir, ig, ib)

type image = { pixels: pixel array
             ; height: int
             ; width: int}

let image2ppm (img: image) : string =
    let sb = new System.Text.StringBuilder()
    let onPixel (r,g,b) =
        sb.Append(string r + " " +
                  string g + " " +
                  string b + "\n")
    ignore (sb.Append("P3\n" +
                      string img.width + " " + string img.height + "\n" +
                      "255\n"))
    for pixel in img.pixels do ignore (onPixel pixel)
    sb.ToString()

let render objs width height cam : image =
    let pixel l =
            let i = l % width
            let j = height - l / width
            in colour_to_pixel (trace_ray objs width height cam j i)
    let pixels = Array.Parallel.init (height*width) pixel
    in { width = width
       ; height = height
       ; pixels = pixels
       }

type scene = { look_from: pos
               look_at: pos
               fov: float
               spheres: sphere list }

let from_scene width height (scene: scene) : objs * camera =
  (mk_bvh sphere_aabb scene.spheres,
   camera scene.look_from scene.look_at {x=0.0; y=1.0; z=0.0}
     scene.fov (float width/float height))

let tabulate_2d m n f =
    Seq.map (fun j -> Seq.map (fun i -> f (j, i)) (seq {0 .. n-1})) (seq {0 .. m-1})
    |> Seq.concat
    |> Seq.toList

let rgbbox : scene =
    let n = 10
    let k = 60.0

    let leftwall =
        tabulate_2d n n (fun (y, z) ->
                            { pos={x=(-k/2.0);
                                   y=(-k/2.0 + (k/float n) * float y);
                                   z=(-k/2.0 + (k/float n) * float z)}
                            ; colour={x=1.0; y=0.0; z=0.0}
                            ; radius = (k/(float n*2.0))})

    let midwall =
        tabulate_2d n n (fun (x,y) ->
                            { pos={x=(-k/2.0 + (k/float n) * float x);
                                   y=(-k/2.0 + (k/float n) * float y);
                                   z=(-k/2.0)}
                            ; colour={x=1.0; y=1.0; z=0.0}
                            ; radius = (k/(float n*2.0))})

    let rightwall =
        tabulate_2d n n (fun (y,z) ->
                            { pos={x=(k/2.0);
                                   y=(-k/2.0 + (k/float n) * float y);
                                   z=(-k/2.0 + (k/float n) * float z)}
                            ; colour={x=0.0; y=0.0; z=1.0}
                            ; radius = (k/(float n*2.0))})


    let bottom =
        tabulate_2d n n (fun (x,z) ->
                            { pos={x=(-k/2.0 + (k/float n) * float x);
                                   y=(-k/2.0);
                                   z=(-k/2.0 + (k/float n) * float z)}
                            ; colour={x=1.0; y=1.0; z=1.0}
                            ; radius = (k/(float n*2.0))})


    in { spheres = leftwall @ midwall @ rightwall @ bottom
       ; look_from = {x=0.0; y=30.0; z=30.0}
       ; look_at = {x=0.0; y= -1.0; z= -1.0}
       ; fov = 75.0 }

let irreg : scene =
    let n = 100
    let k = 600.0
    let bottom =
        tabulate_2d n n (fun (x,z) ->
                            { pos={x=(-k/2.0 + (k/float n) * float x);
                                   y=0.0;
                                   z=(-k/2.0 + (k/float n) * float z)}
                            ; colour = white
                            ; radius = k/(float n * 2.0)})
    in { spheres = bottom
       ; look_from = {x=0.0; y=12.0; z=30.0}
       ; look_at = {x=0.0; y=10.0; z= -1.0}
       ; fov = 75.0 }

let rec getopt needle argv f def =
    match argv with
        | opt::x::xs ->
            if opt = needle
            then f x else getopt needle (x::xs) f def
        | _ -> def

[<EntryPoint>]
let main argv =
    let height = getopt "-m" (Array.toList argv) int 200
    let width = getopt "-n" (Array.toList argv) int 200
    let imgfile = getopt "-f" (Array.toList argv) Some None
    let scene_name = getopt "-s" (Array.toList argv) id "rgbbox"
    let scene =
        match scene_name with
            | "rgbbox" -> rgbbox
            | "irreg" -> irreg
            | s -> failwith ("No such scene: " + s)
    let _ = printfn "Using scene '%s' (-s to switch)." scene_name

    let w = new Stopwatch()

    let _ = w.Restart()
    let (objs, cam) = from_scene width height scene
    let _ = w.Stop()
    let _ = printfn "Scene BVH construction in %fs." w.Elapsed.TotalSeconds

    let _ = w.Restart()
    let result = render objs width height cam
    let _ = w.Stop()
    let _ = printfn "Rendering in %fs." w.Elapsed.TotalSeconds

    let _ =
        match imgfile with
            | None ->
                printfn "-f not passed, so not writing image to file."
            | Some imgfile' ->
                printfn "Writing image to %s." imgfile';
                System.IO.File.WriteAllText(imgfile', image2ppm result)
    0
