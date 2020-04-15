use lazy_static::lazy_static;

use super::{Scene, Sphere, Vec3, WHITE};

lazy_static! {
    pub static ref RGBBOX: Scene = {
        let n = 10;
        let k = 60.0;

        let leftwall: Vec<Sphere> = (0..n)
            .flat_map(|y| {
                (0..n).map(move |z| Sphere {
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
                (0..n).map(move |y| Sphere {
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
                (0..n).map(move |z| Sphere {
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
                (0..n).map(move |z| Sphere {
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
}

lazy_static! {
    pub static ref IRREG: Scene = {
        let n = 100;
        let k = 600.0;
        let spheres = (0..n)
            .flat_map(|x| {
                (0..n).map(move |z| Sphere {
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
}
