#[macro_use]
extern crate structopt;

use std::convert::TryInto;
use std::cmp::{Ordering, PartialOrd};
use std::fs::File;
use std::io;
use std::io::Write;
use std::ops::{Add, Mul, Sub};
use std::time::Instant;

use rayon::prelude::*;

#[derive(Clone, Copy)]
struct Vec3 {
    x: f32,
    y: f32,
    z: f32,
}

impl Vec3 {
    fn scale(self, s: f32) -> Vec3 {
        Vec3 {
            x: self.x * s,
            y: self.y * s,
            z: self.z * s,
        }
    }

    fn norm(&self) -> f32 {
        self.dot(&self).sqrt()
    }

    fn normalise(&self) -> Vec3 {
        self.scale(1.0 / self.norm())
    }

    fn dot(&self, other: &Vec3) -> f32 {
        let v3 = *self * *other;
        v3.x + v3.y + v3.z
    }

    fn cross(&self, other: &Vec3) -> Vec3 {
        Vec3 {
            x: self.y * other.z - self.z * other.y,
            y: self.z * other.x - self.x * other.z,
            z: self.x * other.y - self.y * other.x,
        }
    }

    fn reflect(&self, n: &Vec3) -> Vec3 {
        *self - n.scale(2.0 * self.dot(n))
    }
}

impl Add for Vec3 {
    type Output = Vec3;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl Sub for Vec3 {
    type Output = Vec3;

    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        }
    }
}

impl Mul for Vec3 {
    type Output = Vec3;

    fn mul(self, other: Self) -> Self {
        Self {
            x: self.x * other.x,
            y: self.y * other.y,
            z: self.z * other.z,
        }
    }
}

#[derive(Clone)]
struct Aabb {
    min: Vec3,
    max: Vec3,
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
                tmax3 > tmin3
            }
        }
    }

    fn enclosing(&self, other: &Aabb) -> Aabb {
        let small = Vec3 {
            x: f32::min(self.min.x, other.min.x),
            y: f32::min(self.min.y, other.min.y),
            z: f32::min(self.min.z, other.min.z),
        };
        let big = Vec3 {
            x: f32::max(self.max.x, other.max.x),
            y: f32::max(self.max.y, other.max.y),
            z: f32::max(self.max.z, other.max.z),
        };
        Aabb {
            min: small,
            max: big,
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
        self.origin + self.dir.scale(t)
    }

    fn scatter(&self, hit: &Hit) -> Option<(Ray, Colour)> {
        let reflected = self.dir.normalise().reflect(&hit.normal);
        let scattered = Ray {
            origin: hit.p,
            dir: reflected,
        };
        if scattered.dir.dot(&hit.normal) > 0.0 {
            Some((scattered, hit.colour))
        } else {
            None
        }
    }

    fn colour(&self, objs: &Objs, depth: usize) -> Colour {
        if let Some(hit) = objs.hit(self, 0.001, 1_000_000_000.0) {
            if let Some((scattered, attenuation)) = self.scatter(&hit) {
                if depth < 50 {
                    attenuation * scattered.colour(&objs, depth + 1)
                } else {
                    BLACK
                }
            } else {
                BLACK
            }
        } else {
            let unit_dir = self.dir.normalise();
            let t = 0.5 * (unit_dir.y + 1.0);
            let bg = Vec3 {
                x: 0.5,
                y: 0.7,
                z: 1.0,
            };
            WHITE.scale(1.0 - t) + bg.scale(t)
        }
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
        #![allow(clippy::many_single_char_names)]

        let oc = r.origin - self.pos;
        let a = r.dir.dot(&r.dir);
        let b = oc.dot(&r.dir);
        let c = oc.dot(&oc) - self.radius * self.radius;
        let discriminant = b * b - a * c;

        let helper = |temp| {
            if temp < t_max && temp > t_min {
                Some(Hit {
                    t: temp,
                    p: r.point_at_param(temp),
                    normal: (r.point_at_param(temp) - self.pos).scale(1.0 / self.radius),
                    colour: self.colour,
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
            min: self.pos
                - Vec3 {
                    x: self.radius,
                    y: self.radius,
                    z: self.radius,
                },
            max: self.pos
                + Vec3 {
                    x: self.radius,
                    y: self.radius,
                    z: self.radius,
                },
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
                xs.par_sort_by(|a, b| {
                    let (a_, b_) = match d % 3 {
                        0 => (a.to_aabb().centre().x, b.to_aabb().centre().x),
                        1 => (a.to_aabb().centre().y, b.to_aabb().centre().y),
                        _ => (a.to_aabb().centre().z, a.to_aabb().centre().z),
                    };

                    a_.partial_cmp(&b_).unwrap_or(Ordering::Equal)
                });

                let (xs_left, xs_right) = xs.split_at_mut(n / 2);
                let (left, right) =
                    if n < 100 {
                        (helper(d + 1, n / 2, xs_left),
                         helper(d + 1, n - (n / 2), xs_right))
                    } else {
                        rayon::join(
                            || helper(d + 1, n / 2, xs_left),
                            || helper(d + 1, n - (n / 2), xs_right))
                    };
                let b = left.aabb().enclosing(right.aabb());
                Bvh::Split(b, Box::new(left), Box::new(right))
            }
        }

        helper(0, all_objs.len(), all_objs)
    }
}

type Objs = Bvh<Sphere>;

impl Objs {
    fn hit(&self, r: &Ray, t_min: f32, t_max: f32) -> Option<Hit> {
        match self {
            Bvh::Leaf(_, s) => s.hit(r, t_min, t_max),
            Bvh::Split(b, left, right) => {
                if !(b.hit(r, t_min, t_max)) {
                    None
                } else {
                    match left.hit(r, t_min, t_max) {
                        Some(h) => match right.hit(r, t_min, h.t) {
                            None => Some(h),
                            Some(h_) => Some(h_),
                        },
                        None => right.hit(r, t_min, t_max),
                    }
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
        let w = (*lookfrom - *lookat).normalise();
        let u = vup.cross(&w).normalise();
        let v = w.cross(&u);
        Camera {
            origin: *lookfrom,
            llc: *lookfrom - u.scale(half_width) - v.scale(half_height) - w,

            horizontal: u.scale(2.0 * half_width),
            vertical: v.scale(2.0 * half_height),
        }
    }

    fn ray(&self, s: f32, t: f32) -> Ray {
        Ray {
            origin: self.origin,
            dir: self.llc + self.horizontal.scale(s) + self.vertical.scale(t) - self.origin,
        }
    }
}

fn trace_ray(objs: &Objs, width: i32, height: i32, cam: &Camera, j: i32, i: i32) -> Colour {
    let u = i as f32 / width as f32;
    let v = j as f32 / height as f32;
    let ray = cam.ray(u, v);
    ray.colour(objs, 0)
}

type Pixel = (i32, i32, i32);

impl From<Colour> for Pixel {
    fn from(c: Colour) -> Self {
        (
            (c.x * 255.99) as i32,
            (c.y * 255.99) as i32,
            (c.z * 255.99) as i32,
        )
    }
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
        Pixel::from(trace_ray(objs, width, height, cam, j, i))
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

    #[structopt(short = "r", default_value = "10")]
    /// The number of runs
    runs: i32,

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
                    .map(move |z| Sphere {
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
            })
            .collect();
        let midwall: Vec<Sphere> = (0..n)
            .flat_map(|x| {
                (0..n)
                    .map(move |y| Sphere {
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
            })
            .collect();
        let rightwall: Vec<Sphere> = (0..n)
            .flat_map(|y| {
                (0..n)
                    .map(move |z| Sphere {
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
            })
            .collect();
        let bottom: Vec<Sphere> = (0..n)
            .flat_map(|x| {
                (0..n)
                    .map(move |z| Sphere {
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
                    .map(move |z| Sphere {
                        pos: Vec3 {
                            x: ((-k) / 2.0 + (k / n as f32) * x as f32),
                            y: 0.0,
                            z: ((-k) / 2.0 + (k / n as f32) * z as f32),
                        },
                        colour: WHITE,
                        radius: k / (n as f32 * 2.0),
                    })
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
    println!("Timing over average of {} runs (-r to change).\n", args.runs);

    let start = Instant::now();
    // Run a few times and hope for no dead code elimination.
    for _ in 0..args.runs-1 {
        from_scene(args.width, args.height, &mut scene);
    }
    let (objs, cam) = from_scene(args.width, args.height, &mut scene);
    let duration = start.elapsed()/args.runs.try_into().unwrap();

    println!("Scene BVH construction in {:?}", duration);

    let start = Instant::now();
    // Run a few times and hope for no dead code elimination.
    for _ in 0..args.runs-1 {
        render(&objs, args.width, args.height, &cam);
    }
    let result = render(&objs, args.width, args.height, &cam);
    let duration = start.elapsed();

    println!("Rendering in {:?}", duration);

    if let Some(filename) = args.file_out {
        println!("Writing to {}", filename);
        let file = io::BufWriter::new(File::create(filename)?);
        image2ppm(file, result)?;
    } else {
        println!("Nothing to write");
    }

    Ok(())
}
