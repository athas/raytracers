#[macro_use]
extern crate structopt;

use std::fs::File;
use std::io;
use std::io::Write;
use std::path::Path;

use ray;

#[derive(StructOpt)]
struct Args {
    #[structopt(short = "f")]
    /// The file to output the image to
    file_out: Option<String>,

    #[structopt(short = "m", long = "height", default_value = "200")]
    /// The height
    height: i32,

    #[structopt(short = "n", long = "width", default_value = "200")]
    /// The width
    width: i32,

    #[structopt(short = "s", long = "scene", default_value = "rgbbox")]
    /// The scene to show. Possible values are 'rgbbox' and 'irreg'
    scene_name: String,
}

#[paw::main]
fn main(args: Args) -> Result<(), Box<dyn std::error::Error>> {
    let mut scene = match args.scene_name.as_ref() {
        "irreg" => (*ray::sample_scenes::IRREG).clone(),
        "rgbbox" => (*ray::sample_scenes::RGBBOX).clone(),
        s => panic!("Invalid scene: {}", s),
    };

    let (objs, cam) = ray::from_scene(args.width, args.height, &mut scene);

    let result = ray::render(&objs, args.width, args.height, &cam);

    let out_writer = match args.file_out {
        Some(x) => {
            let path = Path::new(&x);
            Box::new(File::create(&path).unwrap()) as Box<dyn Write>
        }
        None => Box::new(io::stdout()) as Box<dyn Write>,
    };

    ray::image2ppm(out_writer, result)?;

    Ok(())
}
