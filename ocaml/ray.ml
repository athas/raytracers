module Chan = Domainslib.Chan

type vec3 = {
  x : float;
  y : float;
  z : float;
}

let vf f (v1: vec3) (v2: vec3) = {
  x = f v1.x v2.x;
  y = f v1.y v2.y;
  z = f v1.z v2.z
}

let vec_add = vf (+.)
let vec_sub = vf (-.)
let vec_mul = vf ( *. )
let vec_div = vf (/.)

let scale s v : vec3 = {
  x = s *. v.x;
  y = s *. v.y;
  z = s *. v.z;
}

let dot (v1: vec3) (v2: vec3) =
  let v3 = vec_mul v1 v2
  in v3.x +. v3.y +. v3.z

let norm v = sqrt (dot v v)

let normalise v = scale (1.0 /. norm v) v

let cross v1 v2 : vec3 = {
  x = v1.y *. v2.z -. v1.z *. v2.y;
  y = v1.z *. v2.x -. v1.x *. v2.z;
  z = v1.x *. v2.y -. v1.y *. v2.x;
}

type aabb = {
  min : vec3;
  max : vec3
}

let min x y : float =
  if x < y then x else y

let max x y : float =
  if x < y then y else x

let enclosing (box0: aabb) (box1: aabb) =
  let small = {
    x = min box0.min.x box1.min.x;
    y = min box0.min.y box1.min.y;
    z = min box0.min.z box1.min.z;
  }
  and big = {
    x = max box0.max.x box1.max.x;
    y = max box0.max.y box1.max.y;
    z = max box0.max.z box1.max.z;
  }
  in { min = small; max = big }

let centre (aabb: aabb) = {
  x = aabb.min.x +. aabb.max.x -. aabb.min.x;
  y = aabb.min.y +. aabb.max.y -. aabb.min.y;
  z = aabb.min.z +. aabb.max.z -. aabb.min.z;
}

type 'a bvh =
  | Bvh_leaf of aabb * 'a
  | Bvh_split of aabb * 'a bvh * 'a bvh

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

module type ForkJoinSpecs = sig
  val num_domains : int
end

let log s = Printf.printf "%s\n%!" s

module Stack = struct

  type 'a t = 'a list Atomic.t

  let make : unit -> 'a t = fun () -> Atomic.make []

  let nanos = Int64.of_int @@ 0 (*todo play with this*)

  let push : 'a t -> 'a -> unit = fun s v ->
    let rec aux () =
      log "Stack.push.aux entered";
      let success = 
        Domain.Sync.critical_section (fun () ->
          let old_stack = Atomic.get s in
          let new_stack = v :: old_stack in
          Atomic.compare_and_set s old_stack new_stack
        )
      in
      if success then () else (
        Domain.Sync.wait_for nanos |> ignore;
        aux ()
      )
    in
    aux ()

  let filter_pop : 'a t -> ('a -> bool) -> 'a option = fun s p ->
    print_endline "filter_pop entered";
    let rec aux () =
      (* log "Stack.filter_pop.aux entered"; *)
      let result = ref None in
      let success = 
        Domain.Sync.critical_section (fun () ->
          (* print_endline "filter_pop aux entered"; *)
          let old_stack = Atomic.get s in
          match old_stack with
          | head :: new_stack ->
            if not (p head) then (
              log "Stack.filter_pop: stack non-empty, but (p head) = false";
              result := None;
              true
            ) else (
              log "Stack.filter_pop: stack non-empty, and (p head) = true";
              result := Some head;
              Atomic.compare_and_set s old_stack new_stack
            )
          | [] ->
            log "Stack.filter_pop: stack empty";
            result := None;
            true
        )
      in
      if success then !result else aux ()
    in
    aux ()

  let is_empty : 'a t -> bool = fun s ->
    match filter_pop s (fun _ -> true) with None -> true | _ -> false

end

module MakeForkJoin(S:ForkJoinSpecs) = struct

  type work = unit -> unit
  
  effect Par : (work * work) -> unit
  
  let par : type a b. (unit -> a) * (unit -> b) -> (a * b) = fun (f, g) ->
    let res_f = ref None 
    and res_g = ref None in
    let run_f () = res_f := Some (f ())
    and run_g () = res_g := Some (g ()) in
    perform @@ Par (run_f, run_g);
    match !res_f, !res_g with
    | Some fv, Some gv -> fv, gv
    | _ ->
      log "ForkJoin.par: results not set";
      failwith "error"

  type continuation_wrap = (unit -> unit) * (bool ref * bool ref)

  let has_work_done : continuation_wrap -> bool =
    fun (_, (l, r)) -> !l && !r

  let scheduler : type a b. (a -> b) -> a -> b = fun main x ->
    (* let test_done = ref false in (\*todo*\) *)

    let work_queue : work Chan.t = Chan.make @@ 512 * S.num_domains in
    let continuation_stack : continuation_wrap Stack.t = Stack.make () 
    in
    let rec aux = fun f x ->
      log "aux called";
      (* log (Printf.sprintf "test_done = %b" !test_done); *)
      begin match Stack.filter_pop continuation_stack has_work_done with
        | None ->
          log "aux: no work done for continuation";
          ()
        | Some (k, _) ->
          log "aux: work done for continuation!";
          Chan.send work_queue (aux k)
          (*todo: is it really not needed to return result here..
            * thought: the continuation returns to evaluating the function that called 'par'
              * so only need to return from the topmost call
          *)
      end;
      begin match f x with
        | () ->
          log "aux: f x returned!";
          ()
        | effect (Par (work, work')) k ->
          log "aux: Par performed";
          let l_done = ref false in
          let r_done = ref false in
          let set_done f r () = let v = f () in r := true; v in
          Chan.send work_queue (set_done (aux work)  l_done);
          Chan.send work_queue (set_done (aux work') r_done);
          let continuation_wrap =
            let c = fun () -> continue k () |> ignore in
            (c, (l_done, r_done)) in
          Stack.push continuation_stack continuation_wrap;
          log "aux: DONE pushing on stack"
      end
    in
    let rec worker () =
      log "worker entered";
      Chan.recv work_queue (); (*todo blocks when there is no more work *)
      log "worker done work";
      worker () in

    (*todo debug*)
    (* let set_done f r () = let v = f () in r := true; v in
     * Chan.send work_queue (set_done (fun _ -> ()) test_done); *)
      
    let domains : unit Domain.t array =
      Array.init S.num_domains (fun _ -> Domain.spawn worker) in
    let result = ref None in
    let main () = result := Some (main x) in
    aux main ();
    Array.iter Domain.join domains;
    match !result with
    | Some v -> v
    | None -> failwith "ForkJoin.scheduler: result not set"
  
end

let mk_bvh f all_objs =
  let module ForkJoin = MakeForkJoin(struct
    let num_domains = (* 8 *) 1
  end) in
  (*<todo num-domains*)
  let rec mk d n xs =
    match xs with
    | [] -> failwith "mk_bvh: no nodes"
    | [x] -> Bvh_leaf(f x, x)
    | _ ->
      let axis =
        match d mod 3 with
        | 0 -> fun v -> v.x
        | 1 -> fun v -> v.y
        | _ -> fun v -> v.z
      in
      let key x = axis (centre (f x) ) in
      let sort_by_keys x y = compare (key x) (key y) in
      let xs_sorted = List.sort sort_by_keys xs in
      let (xs_left, xs_right) = split (n/2) xs_sorted in
      let do_left () = mk (d+1) (n/2) xs_left in
      let do_right () = mk (d+1) (n-(n/2)) xs_right in
      let (left, right) =
        if n < 100
        then (do_left(), do_right())
        else ForkJoin.par (do_left, do_right)
      in
      let box = enclosing (bvh_aabb left) (bvh_aabb right)
      in Bvh_split (box, left, right)
  in
  ForkJoin.scheduler
    (fun () -> mk 0 (List.length all_objs) all_objs) ()

type pos = vec3
type dir = vec3
type colour = vec3

let black : vec3 = {x=0.0; y=0.0; z=0.0}
let white : vec3 = {x=1.0; y=1.0; z=1.0}

type ray = {origin: pos; dir: dir}

let point_at_param (ray: ray) t =
  vec_add ray.origin (scale t ray.dir)

type hit = {
  t: float;
  p: pos;
  normal: dir;
  colour: colour;
}

type sphere = {
  pos: pos;
  colour: colour;
  radius: float;
}

let sphere_aabb (s: sphere) : aabb =
  { min = vec_sub s.pos {x=s.radius; y=s.radius; z=s.radius}
  ; max = vec_add s.pos {x=s.radius; y=s.radius; z=s.radius}}

let sphere_hit s r t_min t_max : hit option =
  let oc = vec_sub r.origin s.pos in
  let a = dot r.dir r.dir in
  let b = dot oc r.dir in
  let c = dot oc oc -. s.radius *. s.radius in
  let discriminant = b *. b -. a *. c in
  let f temp =
    if temp < t_max && temp > t_min
    then Some { t = temp
              ; p = point_at_param r temp
              ; normal = scale (1.0 /. s.radius)
                  (vec_sub (point_at_param r temp) s.pos)
              ; colour = s.colour
    }
    else None
  in if discriminant <= 0.0
  then None
  else
    let sqrt_v = sqrt (b *. b -. a *. c) in
    match f ((-.b -. sqrt_v) /. a) with
    | Some hit -> Some hit
    | None -> f ((-.b +. sqrt_v) /. a)

let aabb_hit aabb (r: ray) tmin0 tmax0 =
  let iter min' max' origin' dir' tmin' tmax' =
    let invD = 1.0 /. dir' in
    let t0 = (min' -. origin') *. invD in
    let t1 = (max' -. origin') *. invD in
    let (t0', t1') = if invD < 0.0 then (t1, t0) else (t0, t1) in
    let tmin'' = max t0' tmin' in
    let tmax'' = min t1' tmax' in
    (tmin'', tmax'')
  in
  let (tmin1, tmax1) =
    iter aabb.min.x aabb.max.x r.origin.x r.dir.x tmin0 tmax0
  in
  if tmax1 <= tmin1 then false
  else
    let (tmin2, tmax2) =
      iter aabb.min.y aabb.max.y r.origin.y r.dir.y tmin1 tmax1
    in
    if tmax2 <= tmin2 then false
    else
      let (tmin3, tmax3) =
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

type camera = {
  origin: pos;
  llc: pos;
  horizontal: dir;
  vertical: dir;
}

let pi = 3.14159265358979312

let camera lookfrom lookat vup vfov aspect =
  let theta = vfov *. pi /. 180. in
  let half_height = tan (theta /. 2.) in
  let half_width = aspect *. half_height in
  let origin = lookfrom in
  let w = normalise (vec_sub lookfrom lookat) in
  let u = normalise (cross vup w) in
  let v = cross w u 
  in
  { origin = lookfrom;
    llc = vec_sub
        (vec_sub (vec_sub origin (scale half_width u))
            (scale half_height v)) w;
    horizontal = scale (2. *. half_width) u;
    vertical = scale (2. *. half_height) v;
  }

let get_ray (cam: camera) s t : ray =
  { origin = cam.origin
  ; dir =
      vec_sub
        (vec_add
            (vec_add cam.llc (scale s cam.horizontal))
            (scale t cam.vertical))
        cam.origin
  }

let reflect v n =
  vec_sub v (scale (2. *. dot v n) n)

let scatter (r: ray) (hit: hit) =
  let reflected = reflect (normalise r.dir) hit.normal in
  let scattered = {origin = hit.p; dir = reflected} 
  in
  if dot scattered.dir hit.normal > 0.0
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
    let unit_dir = normalise r.dir in
    let t = 0.5 *. (unit_dir.y +. 1.0) in
    let bg = { x = 0.5; y = 0.7; z = 1.0}
    in vec_add (scale (1.0 -. t) white) (scale t bg)

let trace_ray objs width height cam j i : colour =
  let u = float i /. float width in
  let v = float j /. float height in
  let ray = get_ray cam u v 
  in ray_colour objs ray 0

type pixel = int * int * int

let colour_to_pixel p =
  let ir = int_of_float (255.99 *. p.x) in
  let ig = int_of_float (255.99 *. p.y) in
  let ib = int_of_float (255.99 *. p.z) 
  in (ir, ig, ib)

type image = {
  pixels: pixel array;
  height: int;
  width: int;
}

let sp = Printf.sprintf

let image2ppm : image -> string = fun image -> 
  let on_pixel acc (r, g, b) = sp "%d %d %d\n" r g b :: acc in
  String.concat "" @@ List.rev_append (*< is tailrec in contrast with 'flatten'*) 
    (List.rev [
      "P3\n";
      sp "%d %d\n" image.width image.height;
      "255\n";
    ])
    (image.pixels |> Array.to_list |> List.fold_left on_pixel [] |> List.rev)

module Workers = struct 

  (** Helpers for managing domain workers, derived from the following Multicore 
      OCaml benchmark:
      https://github.com/ocaml-bench/sandmark/blob/master/benchmarks/multicore-grammatrix/grammatrix_multicore.ml
  *)

  module StaticInput = struct 
  
    type message =
      | Work of int (*start*) * int (*stop*)
      | Quit

    let create_work_queue ~n ~chunk_size ~num_domains =
      Chan.make (
        n / chunk_size +
        1 + (*remaining work*)
        num_domains (*quit messages*)
      )    

    let create_work ~n ~num_domains ~chunk_size =
      let chan = create_work_queue ~n ~chunk_size ~num_domains in
      let rec aux ~start ~left = 
        if left < chunk_size then begin
          Chan.send chan (Work (start, start + left - 1));
          for _i = 1 to num_domains do
            Chan.send chan Quit
          done
        end else begin
          Chan.send chan (Work (start, start + chunk_size - 1));
          aux ~start:(start + chunk_size) ~left:(left - chunk_size)
        end
      in
      aux ~start:0 ~left:n;
      chan

    let rec worker ~f ~input ~output ~work_queue =
      match Chan.recv work_queue with
      | Work (start, stop) ->
        f ~input ~output ~start ~stop;
        worker ~f ~input ~output ~work_queue
      | Quit -> ()

  end

end

let render ~objs ~width ~height ~cam ~num_domains ~chunk_size =
  let pixel l =
    let i = l mod width in
    let j = height - l / width 
    in colour_to_pixel (trace_ray objs width height cam j i)
  in
  let pixel_work ~input ~output ~start ~stop =
    for i = start to stop do output.(i) <- pixel i done
  in
  let n = height * width in
  let output = Array.make n (0, 0, 0) in
  let module W = Workers.StaticInput in
  begin
    let work_queue = W.create_work ~n ~num_domains ~chunk_size in
    let domains =
      Array.init (num_domains - 1) (fun _ ->
        Domain.spawn
          (fun _ -> W.worker ~f:pixel_work ~input:() ~output ~work_queue))
    in
    W.worker ~f:pixel_work ~input ~output ~work_queue;
    Array.iter Domain.join domains;
  end;
  {
    width;
    height;
    pixels = output
  }

type scene = {
  look_from : pos;
  look_at : pos;
  fov : float;
  spheres : sphere list;
}

let from_scene width height (scene: scene) : objs * camera =
  (mk_bvh sphere_aabb scene.spheres,
   camera scene.look_from scene.look_at {x=0.0; y=1.0; z=0.0}
     scene.fov (float width /. float height))

(*taken from a later OCaml version*)
module Seq = struct 

  type +'a node =
    | Nil
    | Cons of 'a * 'a t

  and 'a t = unit -> 'a node

  let empty () = Nil
  
  let rec map f seq () = match seq() with
    | Nil -> Nil
    | Cons (x, next) -> Cons (f x, map f next)

  let rec flat_map f seq () = match seq () with
    | Nil -> Nil
    | Cons (x, next) ->
      flat_map_app f (f x) next ()

  (* this is [append seq (flat_map f tail)] *)
  and flat_map_app f seq tail () = match seq () with
    | Nil -> flat_map f tail ()
    | Cons (x, next) ->
      Cons (x, flat_map_app f next tail)

  let fold_left f acc seq =
    let rec aux f acc seq = match seq () with
      | Nil -> acc
      | Cons (x, next) ->
        let acc = f acc x in
        aux f acc next
    in
    aux f acc seq

end

let seq_range x y : int Seq.t =
  let rec aux x () =
    if x > y then Seq.Nil else
      Seq.Cons (x, aux (succ x))
  in
  aux x

(*taken from a later OCaml version*)
let list_of_seq seq =
  let rec direct depth seq : _ list =
    if depth=0
    then
      Seq.fold_left (fun acc x -> x::acc) [] seq
      |> List.rev (* tailrec *)
    else match seq() with
      | Seq.Nil -> []
      | Seq.Cons (x, next) -> x :: direct (depth-1) next
  in
  direct 500 seq

let tabulate_2d m n f =
  Seq.flat_map (fun j ->
    Seq.map (fun i ->
      f (j, i)
    ) (seq_range 0 (n-1))
  ) (seq_range 0 (m-1))
  |> list_of_seq

let rgbbox : scene =
  let n = 10 in
  let k = 60.0 
  in
  let leftwall =
    tabulate_2d n n (fun (y, z) ->
      { pos={x=(-.k/.2.0);
          y=(-.k/.2.0 +. (k/.float n) *. float y);
          z=(-.k/.2.0 +. (k/.float n) *. float z)}
      ; colour={x=1.0; y=0.0; z=0.0}
      ; radius = (k/.(float n*.2.0))})
  in
  let midwall =
    tabulate_2d n n (fun (x,y) ->
      { pos={x=(-.k/.2.0 +. (k/.float n) *. float x);
          y=(-.k/.2.0 +. (k/.float n) *. float y);
          z=(-.k/.2.0)}
      ; colour={x=1.0; y=1.0; z=0.0}
      ; radius = (k/.(float n*.2.0))})
  in
  let rightwall =
    tabulate_2d n n (fun (y,z) ->
      { pos={x=(k/.2.0);
          y=(-.k/.2.0 +. (k/.float n) *. float y);
          z=(-.k/.2.0 +. (k/.float n) *. float z)}
      ; colour={x=0.0; y=0.0; z=1.0}
      ; radius = (k/.(float n*.2.0))})
  in
  let bottom =
    tabulate_2d n n (fun (x,z) ->
      { pos={x=(-.k/.2.0 +. (k/.float n) *. float x);
          y=(-.k/.2.0);
          z=(-.k/.2.0 +. (k/.float n) *. float z)}
      ; colour={x=1.0; y=1.0; z=1.0}
      ; radius = (k/.(float n*.2.0))})
  in
  {
    spheres = leftwall @ midwall @ rightwall @ bottom;
    look_from = {x=0.0; y=30.0; z=30.0};
    look_at = {x=0.0; y= -.1.0; z= -.1.0};
    fov = 75.0
  }

let irreg : scene =
  let n = 100 in
  let k = 600.0 in
  let bottom =
    tabulate_2d n n (fun (x,z) ->
      { pos={x=(-.k/.2.0 +. (k/.float n) *. float x);
          y=0.0;
          z=(-.k/.2.0 +. (k/.float n) *. float z)}
      ; colour = white
      ; radius = k/.(float n *. 2.0)})
  in { spheres = bottom
     ; look_from = {x=0.0; y=12.0; z=30.0}
     ; look_at = {x=0.0; y=10.0; z= -.1.0}
     ; fov = 75.0 }

let rec getopt needle argv f def =
  match argv with
  | opt::x::xs ->
    if opt = needle
    then f x else getopt needle (x::xs) f def
  | _ -> def

let some x = Some x
let id x = x

external useconds : unit -> int = "useconds"

let seconds() = float_of_int (useconds()) /. 1000000.0

let () =
  let argv = Sys.argv |> Array.to_list in

  let num_domains = getopt "--cores" argv int_of_string 8 in
  let chunk_size_render = getopt "--chunk-size-render" argv int_of_string 256 in

  let height = getopt "-m" argv int_of_string 200 in
  let width = getopt "-n" argv int_of_string 200 in
  let imgfile = getopt "-f" argv some None in
  let scene_name = getopt "-s" argv id "rgbbox" in
  let scene =
    match scene_name with
    | "rgbbox" -> rgbbox
    | "irreg" -> irreg
    | s -> failwith ("No such scene: " ^ s) in
  let _ = Printf.printf "Using scene '%s' (-s to switch).\n" scene_name in
  (*Note: Unix module was not implemented in Multicore OCaml, 
    .. and Sys.time times all threads time accumulated?*)

  let t = seconds() in
  let (objs, cam) = from_scene width height scene in
  let t' = seconds() in
  let _ = Printf.printf "Scene BVH construction in %fs.\n" (t' -. t) in

  let t = seconds() in
  let result =
    render
      ~num_domains ~chunk_size:chunk_size_render
      ~objs ~width ~height ~cam
  in
  let t' = seconds() in
  let _ = Printf.printf "Rendering in %fs.\n" (t' -. t) in

  match imgfile with
  | None ->
    Printf.printf "-f not passed, so not writing image to file.\n"
  | Some imgfile' ->
    Printf.printf "Writing image to %s.\n" imgfile';
    let out_channel = open_out imgfile' in
    image2ppm result |> output_string out_channel;
    flush out_channel;
    exit 0



