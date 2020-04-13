#[macro_use]
extern crate structopt;

use std::cmp::{Ordering, PartialOrd};
use std::fs::File;
use std::io;
use std::io::Write;
use std::time::Instant;

use rayon::prelude::*;

#[derive(Clone)]
struct Vec3 {
    x: f32,
    y: f32,
    z: f32,
}

fn vec_add(v1: &Vec3, v2: &Vec3) -> Vec3 {
    Vec3 {
        x: v1.x + v2.x,
        y: v1.y + v2.y,
        z: v1.z + v2.z,
    }
}

fn vec_sub(v1: &Vec3, v2: &Vec3) -> Vec3 {
    Vec3 {
        x: v1.x - v2.x,
        y: v1.y - v2.y,
        z: v1.z - v2.z,
    }
}

fn vec_mul(v1: &Vec3, v2: &Vec3) -> Vec3 {
    Vec3 {
        x: v1.x * v2.x,
        y: v1.y * v2.y,
        z: v1.z * v2.z,
    }
}

fn scale(s: f32, v: &Vec3) -> Vec3 {
    Vec3 {
        x: v.x * s,
        y: v.y * s,
        z: v.z * s,
    }
}

fn dot(v1: &Vec3, v2: &Vec3) -> f32 {
    let v3 = vec_mul(v1, v2);
    v3.x + v3.y + v3.z
}

fn norm(v: &Vec3) -> f32 {
    f32::sqrt(dot(v, v))
}

fn normalise(v: &Vec3) -> Vec3 {
    scale(1.0 / norm(v), v)
}

fn cross(v1: &Vec3, v2: &Vec3) -> Vec3 {
    Vec3 {
        x: v1.y * v2.z - v1.z * v2.y,
        y: v1.z * v2.x - v1.x * v2.z,
        z: v1.x * v2.y - v1.y * v2.x,
    }
}

#[derive(Clone)]
struct Aabb {
    min: Vec3,
    max: Vec3,
}

fn enclosing(box0: &Aabb, box1: &Aabb) -> Aabb {
    let small = Vec3 {
        x: f32::min(box0.min.x, box1.min.x),
        y: f32::min(box0.min.y, box1.min.y),
        z: f32::min(box0.min.z, box1.min.z),
    };
    let big = Vec3 {
        x: f32::max(box0.max.x, box1.max.x),
        y: f32::max(box0.max.y, box1.max.y),
        z: f32::max(box0.max.z, box1.max.z),
    };
    Aabb {
        min: small,
        max: big,
    }
}

impl Aabb {
    fn centre(&self) -> Vec3 {
        Vec3 {
            x: self.min.x + (self.max.x - self.min.x),
            y: self.min.y + (self.max.y - self.min.y),
            z: self.min.z + (self.max.z - self.min.z),
        }
    }

    fn hit(&self, r: &Ray, tmin0: f32, tmax0: f32) -> bool {
        let iter = |min_, max_, origin_, dir_, tmin_, tmax_| {
            let inv_d = 1.0 / dir_;
            let t0: f32 = (min_ - origin_) * inv_d;
            let t1 = (max_ - origin_) * inv_d;
            let (t0_, t1_) = if inv_d < 0.0 { (t1, t0) } else { (t0, t1) };
            let tmin__ = t0_.max(tmin_);
            let tmax__ = t1_.min(tmax_);
            (tmin__, tmax__)
        };
        let (tmin1, tmax1) = iter(self.min.x, self.max.x, r.origin.x, r.dir.x, tmin0, tmax0);
        if tmax1 <= tmin1 {
            false
        } else {
            let (tmin2, tmax2) = iter(self.min.y, self.max.y, r.origin.y, r.dir.y, tmin1, tmax1);
            if tmax2 <= tmin2 {
                false
            } else {
                let (tmin3, tmax3) =
                    iter(self.min.z, self.max.z, r.origin.z, r.dir.z, tmin2, tmax2);
                !(tmax3 <= tmin3)
            }
        }
    }
}

enum Bvh<T> {
    Leaf(Aabb, T),
    Split(Aabb, Box<Bvh<T>>, Box<Bvh<T>>),
}

impl<T> Bvh<T> {
    fn aabb(&self) -> &Aabb {
        match self {
            Bvh::Leaf(aabb, _) => aabb,
            Bvh::Split(aabb, _, _) => aabb,
        }
    }
}

type Pos = Vec3;

type Dir = Vec3;

type Colour = Vec3;

const BLACK: Colour = Vec3 {
    x: 0.0,
    y: 0.0,
    z: 0.0,
};
const WHITE: Colour = Vec3 {
    x: 1.0,
    y: 1.0,
    z: 1.0,
};

struct Ray {
    origin: Pos,
    dir: Dir,
}

impl Ray {
    fn point_at_param(&self, t: f32) -> Vec3 {
        vec_add(&self.origin, &scale(t, &self.dir))
    }
}

struct Hit {
    t: f32,
    p: Pos,
    normal: Dir,
    colour: Colour,
}

#[derive(Clone)]
struct Sphere {
    pos: Pos,
    colour: Colour,
    radius: f32,
}

impl Sphere {
    fn hit(&self, r: &Ray, t_min: f32, t_max: f32) -> Option<Hit> {
        let oc = vec_sub(&r.origin, &self.pos);
        let a = dot(&r.dir, &r.dir);
        let b = dot(&oc, &r.dir);
        let c = dot(&oc, &oc) - self.radius * self.radius;
        let discriminant = b * b - a * c;

        let helper = |temp| {
            if temp < t_max && temp > t_min {
                Some(Hit {
                    t: temp,
                    p: r.point_at_param(temp),
                    normal: scale(
                        1.0 / self.radius,
                        &vec_sub(&r.point_at_param(temp), &self.pos),
                    ),
                    colour: self.colour.clone(),
                })
            } else {
                None
            }
        };

        if discriminant <= 0.0 {
            None
        } else {
            match helper((-b - (b * b - a * c).sqrt()) / a) {
                None => helper((-b + (b * b - a * c).sqrt()) / a),
                x => x,
            }
        }
    }
}

trait ToAabb {
    fn to_aabb(&self) -> Aabb;
}

impl ToAabb for Sphere {
    fn to_aabb(&self) -> Aabb {
        Aabb {
            min: vec_sub(
                &self.pos,
                &Vec3 {
                    x: self.radius,
                    y: self.radius,
                    z: self.radius,
                },
            ),
            max: vec_add(
                &self.pos,
                &Vec3 {
                    x: self.radius,
                    y: self.radius,
                    z: self.radius,
                },
            ),
        }
    }
}

impl<T> Bvh<T> {
    fn new(all_objs: &mut [T]) -> Self
    where
        T: Send + ToAabb + Clone,
    {
        fn helper<T>(d: i32, n: usize, xs: &mut [T]) -> Bvh<T>
        where
            T: Send + ToAabb + Clone,
        {
            if xs.is_empty() {
                panic!("No nodes")
            } else if xs.len() == 1 {
                Bvh::Leaf(xs[0].to_aabb(), xs[0].clone())
            } else {
                xs.sort_by(|a, b| {
                    let (a_, b_) = match d % 3 {
                        0 => (a.to_aabb().centre().x, b.to_aabb().centre().x),
                        1 => (a.to_aabb().centre().y, b.to_aabb().centre().y),
                        _ => (a.to_aabb().centre().z, a.to_aabb().centre().z),
                    };

                    a_.partial_cmp(&b_).unwrap_or(Ordering::Equal)
                });

                let (xs_left, xs_right) = xs.split_at_mut(n / 2);
                let (left, right) = rayon::join(
                    || helper(d + 1, n / 2, xs_left),
                    || helper(d + 1, n - (n / 2), xs_right),
                );
                let b = enclosing(left.aabb(), right.aabb());
                Bvh::Split(b, Box::new(left), Box::new(right))
            }
        }

        helper(0, all_objs.len(), all_objs)
    }
}

type Objs = Bvh<Sphere>;

// Kan laves til impl
fn objs_hit(bvh: &Objs, r: &Ray, t_min: f32, t_max: f32) -> Option<Hit> {
    match bvh {
        Bvh::Leaf(_, s) => s.hit(r, t_min, t_max),
        Bvh::Split(b, left, right) => {
            if !(b.hit(r, t_min, t_max)) {
                None
            } else {
                match objs_hit(&*left, r, t_min, t_max) {
                    Some(h) => match objs_hit(&*right, r, t_min, h.t) {
                        None => Some(h),
                        Some(h_) => Some(h_),
                    },
                    None => objs_hit(&*right, r, t_min, t_max),
                }
            }
        }
    }
}

struct Camera {
    origin: Pos,
    llc: Pos,
    horizontal: Dir,
    vertical: Dir,
}

impl Camera {
    fn new(lookfrom: &Pos, lookat: &Pos, vup: &Vec3, vfov: f32, aspect: f32) -> Self {
        let theta = vfov * std::f32::consts::PI / 180.0;
        let half_height = (theta / 2.0).tan();
        let half_width = aspect * half_height;
        let w = normalise(&vec_sub(lookfrom, lookat));
        let u = normalise(&cross(vup, &w));
        let v = cross(&w, &u);
        Camera {
            origin: lookfrom.clone(),
            llc: vec_sub(
                &vec_sub(
                    &vec_sub(lookfrom, &scale(half_width, &u)),
                    &scale(half_height, &v),
                ),
                &w,
            ),
            horizontal: scale(2.0 * half_width, &u),
            vertical: scale(2.0 * half_height, &v),
        }
    }

    fn ray(&self, s: f32, t: f32) -> Ray {
        Ray {
            origin: self.origin.clone(),
            dir: vec_sub(
                &vec_add(
                    &vec_add(&self.llc, &scale(s, &self.horizontal)),
                    &scale(t, &self.vertical),
                ),
                &self.origin,
            ),
        }
    }
}

fn reflect(v: &Vec3, n: &Vec3) -> Vec3 {
    vec_sub(v, &scale(2.0 * dot(v, n), n))
}

fn scatter(r: &Ray, hit: &Hit) -> Option<(Ray, Colour)> {
    let reflected = reflect(&normalise(&r.dir), &hit.normal);
    let scattered = Ray {
        origin: hit.p.clone(),
        dir: reflected,
    };
    if dot(&scattered.dir, &hit.normal) > 0.0 {
        Some((scattered, hit.colour.clone()))
    } else {
        None
    }
}

fn ray_colour(objs: &Objs, r: &Ray, depth: usize) -> Colour {
    if let Some(hit) = objs_hit(objs, r, 0.001, 1_000_000_000.0) {
        if let Some((scattered, attenuation)) = scatter(r, &hit) {
            if depth < 50 {
                vec_mul(&attenuation, &ray_colour(&objs, &scattered, depth + 1))
            } else {
                BLACK
            }
        } else {
            BLACK
        }
    } else {
        let unit_dir = normalise(&r.dir);
        let t = 0.5 * (unit_dir.y + 1.0);
        let bg = Vec3 {
            x: 0.5,
            y: 0.7,
            z: 1.0,
        };
        vec_add(&scale(1.0 - t, &WHITE), &scale(t, &bg))
    }
}

fn trace_ray(objs: &Objs, width: i32, height: i32, cam: &Camera, j: i32, i: i32) -> Colour {
    let u = i as f32 / width as f32;
    let v = j as f32 / height as f32;
    let ray = cam.ray(u, v);
    ray_colour(objs, &ray, 0)
}

type Pixel = (i32, i32, i32);

fn colour_to_pixel(c: Colour) -> Pixel {
    (
        (c.x * 255.99) as i32,
        (c.y * 255.99) as i32,
        (c.z * 255.99) as i32,
    )
}

struct Image {
    pixels: Vec<Pixel>,
    height: i32,
    width: i32,
}

fn image2ppm(mut buf: impl Write, image: Image) -> io::Result<()> {
    write!(buf, "P3\n{} {}\n255\n", image.width, image.height)?;

    for (r, g, b) in image.pixels.iter() {
        writeln!(buf, "{} {} {}", r, g, b)?;
    }

    Ok(())
}

fn render(objs: &Objs, width: i32, height: i32, cam: &Camera) -> Image {
    let pixel = |l| {
        let i = l % width;
        let j = height - l / width;
        colour_to_pixel(trace_ray(objs, width, height, cam, j, i))
    };

    let pixels: Vec<_> = (0..height * width).into_par_iter().map(pixel).collect();

    Image {
        pixels,
        height,
        width,
    }
}

struct Scene {
    cam_look_from: Pos,
    cam_look_at: Pos,
    cam_fov: f32,
    spheres: Vec<Sphere>,
}

fn from_scene(width: i32, height: i32, scene: &mut Scene) -> (Objs, Camera) {
    (
        Bvh::new(&mut scene.spheres),
        Camera::new(
            &scene.cam_look_from,
            &scene.cam_look_at,
            &Vec3 {
                x: 0.0,
                y: 1.0,
                z: 0.0,
            },
            scene.cam_fov,
            width as f32 / height as f32,
        ),
    )
}

#[derive(StructOpt)]
struct Args {
    #[structopt(short = "f")]
    /// The file to output the image to
    file_out: Option<String>,

    #[structopt(short = "m", default_value = "200")]
    /// The height
    height: i32,

    #[structopt(short = "n", default_value = "200")]
    /// The width
    width: i32,

    #[structopt(short = "s", default_value = "rgbbox")]
    /// The scene to show. Possible values are 'rgbbox' and 'irreg'
    scene_name: String,
}

#[paw::main]
fn main(args: Args) -> Result<(), Box<dyn std::error::Error>> {
    let rgbbox = {
        let n = 10;
        let k = 60.0;

        let leftwall: Vec<Sphere> = (0..n)
            .flat_map(|y| {
                (0..n)
                    .map(|z| Sphere {
                        pos: Vec3 {
                            x: (-k) / 2.0,
                            y: ((-k) / 2.0 + (k / n as f32) * y as f32),
                            z: ((-k) / 2.0 + (k / n as f32) * z as f32),
                        },
                        colour: Vec3 {
                            x: 1.0,
                            y: 0.0,
                            z: 0.0,
                        },
                        radius: k / (n as f32 * 2.0),
                    })
                    .collect::<Vec<Sphere>>()
            })
            .collect();
        let midwall: Vec<Sphere> = (0..n)
            .flat_map(|x| {
                (0..n)
                    .map(|y| Sphere {
                        pos: Vec3 {
                            x: ((-k) / 2.0 + (k / n as f32) * x as f32),
                            y: ((-k) / 2.0 + (k / n as f32) * y as f32),
                            z: (-k) / 2.0,
                        },
                        colour: Vec3 {
                            x: 1.0,
                            y: 1.0,
                            z: 0.0,
                        },
                        radius: k / (n as f32 * 2.0),
                    })
                    .collect::<Vec<Sphere>>()
            })
            .collect();
        let rightwall: Vec<Sphere> = (0..n)
            .flat_map(|y| {
                (0..n)
                    .map(|z| Sphere {
                        pos: Vec3 {
                            x: k / 2.0,
                            y: ((-k) / 2.0 + (k / n as f32) * y as f32),
                            z: ((-k) / 2.0 + (k / n as f32) * z as f32),
                        },
                        colour: Vec3 {
                            x: 0.0,
                            y: 0.0,
                            z: 1.0,
                        },
                        radius: k / (n as f32 * 2.0),
                    })
                    .collect::<Vec<Sphere>>()
            })
            .collect();
        let bottom: Vec<Sphere> = (0..n)
            .flat_map(|x| {
                (0..n)
                    .map(|z| Sphere {
                        pos: Vec3 {
                            x: ((-k) / 2.0 + (k / n as f32) * x as f32),
                            y: (-k) / 2.0,
                            z: ((-k) / 2.0 + (k / n as f32) * z as f32),
                        },
                        colour: Vec3 {
                            x: 1.0,
                            y: 1.0,
                            z: 1.0,
                        },
                        radius: k / (n as f32 * 2.0),
                    })
                    .collect::<Vec<Sphere>>()
            })
            .collect();
        Scene {
            spheres: [leftwall, midwall, rightwall, bottom].concat(),
            cam_look_from: Vec3 {
                x: 0.0,
                y: 30.0,
                z: 30.0,
            },
            cam_look_at: Vec3 {
                x: 0.0,
                y: -1.0,
                z: -1.0,
            },
            cam_fov: 75.0,
        }
    };

    let irreg = {
        let n = 100;
        let k = 600.0;
        let spheres = (0..n)
            .flat_map(|x| {
                (0..n)
                    .map(|z| Sphere {
                        pos: Vec3 {
                            x: ((-k) / 2.0 + (k / n as f32) * x as f32),
                            y: 0.0,
                            z: ((-k) / 2.0 + (k / n as f32) * z as f32),
                        },
                        colour: WHITE,
                        radius: k / (n as f32 * 2.0),
                    })
                    .collect::<Vec<Sphere>>()
            })
            .collect();

        Scene {
            spheres,
            cam_look_from: Vec3 {
                x: 0.0,
                y: 12.0,
                z: 30.0,
            },
            cam_look_at: Vec3 {
                x: 0.0,
                y: 10.0,
                z: -1.0,
            },
            cam_fov: 75.0,
        }
    };

    let mut scene = match args.scene_name.as_ref() {
        "irreg" => irreg,
        "rgbbox" => rgbbox,
        s => panic!("Invalid scene: {}", s),
    };

    println!("Using scene '{}'", args.scene_name);

    let start = Instant::now();
    let (objs, cam) = from_scene(args.width, args.height, &mut scene);
    let duration = start.elapsed();

    println!("Scene BVH construction in {:?}", duration);

    let start = Instant::now();
    let result = render(&objs, args.width, args.height, &cam);
    let duration = start.elapsed();

    println!("Rendering in {:?}", duration);

    if let Some(filename) = args.file_out {
        println!("Writing to {}", filename);
        let file = File::create(filename)?;
        image2ppm(file, result)?;
    } else {
        println!("Nothing to write");
    }

    Ok(())
}
